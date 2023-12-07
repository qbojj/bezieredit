#include <glad.h>

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <GLFW/glfw3.h>

#include <imgui.h>
#include <backends/imgui_impl_glfw.h>
#include <backends/imgui_impl_opengl3.h>

#include <rapidjson/document.h>
#include <rapidjson/prettywriter.h>
#include <rapidjson/istreamwrapper.h>
#include <rapidjson/ostreamwrapper.h>
#include <rapidjson/error/en.h>

#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <stdexcept>
#include <array>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <functional>
#include <memory>
#include <list>
#include <iostream>
#include <fstream>

class Shader
{
public:
    Shader(const char *vertex_shader_source, const char *fragment_shader_source)
    {
        vertex_shader = compile_shader(GL_VERTEX_SHADER, vertex_shader_source);
        fragment_shader = compile_shader(GL_FRAGMENT_SHADER, fragment_shader_source);
        program = link_program(vertex_shader, fragment_shader);
    }

    ~Shader()
    {
        glDeleteShader(vertex_shader);
        glDeleteShader(fragment_shader);
        glDeleteProgram(program);
    }

    void use()
    {
        glUseProgram(program);
    }

    Shader(const Shader &) = delete;
    Shader &operator=(const Shader &) = delete;

private:
    GLuint vertex_shader;
    GLuint fragment_shader;
    GLuint program;

    static GLuint compile_shader(GLenum type, const char *source)
    {
        GLuint shader = glCreateShader(type);
        glShaderSource(shader, 1, &source, NULL);
        glCompileShader(shader);

        GLint status;
        glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
        if (status != GL_TRUE)
        {
            char buffer[512];
            glGetShaderInfoLog(shader, 512, NULL, buffer);
            printf("Failed to compile shader: %s\n", buffer);
            exit(1);
        }

        return shader;
    }

    static GLuint link_program(GLuint vertex_shader, GLuint fragment_shader)
    {
        GLuint program = glCreateProgram();
        glAttachShader(program, vertex_shader);
        glAttachShader(program, fragment_shader);
        glLinkProgram(program);

        GLint status;
        glGetProgramiv(program, GL_LINK_STATUS, &status);
        if (status != GL_TRUE)
        {
            char buffer[512];
            glGetProgramInfoLog(program, 512, NULL, buffer);
            printf("Failed to link program: %s\n", buffer);
            exit(1);
        }

        return program;
    }
};

class BezierShader : public Shader
{
public:
    BezierShader() : Shader(bezier_vertex_shader, bezier_fragment_shader)
    {
        glGenVertexArrays(1, &vao);
    }

    ~BezierShader()
    {
        glDeleteVertexArrays(1, &vao);
    }

    void draw(const glm::vec2 &p0, const glm::vec2 &p1, const glm::vec2 &p2, const glm::vec2 &p3, int subdivision, ImU32 color)
    {
        use();

        glUniformMatrix4fv(0, 1, GL_FALSE, &MVP[0][0]);

        float data[4][2]{
            {p0.x, p0.y},
            {p1.x, p1.y},
            {p2.x, p2.y},
            {p3.x, p3.y},
        };
        glUniform2fv(4, 4, &data[0][0]);

        glUniform4f(8, (color & 0xff) / 255.0f,
                    ((color >> 8) & 0xff) / 255.0f,
                    ((color >> 16) & 0xff) / 255.0f,
                    ((color >> 24) & 0xff) / 255.0f);
        glUniform1i(9, subdivision);

        glBindVertexArray(vao);
        glDrawArrays(GL_LINE_STRIP, 0, subdivision);
    }

    glm::mat4 MVP;

    static BezierShader &get()
    {
        static BezierShader instance;
        return instance;
    }

private:
    GLuint vao;

    static constexpr const char bezier_vertex_shader[] = R"(
#version 420
#extension GL_ARB_explicit_uniform_location : require

layout(location = 0) uniform mat4 MVP;
layout(location = 4) uniform vec2 control_points[4];
layout(location = 9) uniform int subdivision;

void main() {
    float t = float(gl_VertexID) / (subdivision - 1);
    float t1 = 1 - t;
    vec2 pos = (t * t * t) * control_points[0] +
                (3 * t1 * t * t) * control_points[1] +
                (3 * t1 * t1 * t) * control_points[2] +
                (t1 * t1 * t1) * control_points[3];
    
    gl_Position = MVP * vec4(pos, 0.0f, 1.0f);
})";

    static constexpr const char bezier_fragment_shader[] = R"(
#version 420
#extension GL_ARB_explicit_uniform_location : require

layout(location = 8) uniform vec4 color;
layout(location = 0) out vec4 frag_color;

void main() {
    frag_color = color;
})";
};

struct control_points_t
{
    int add_point(const glm::vec2 &pos)
    {
        int id = next_id++;
        points[id] = {pos, 1};
        return id;
    }

    void remove_point(int id)
    {
        if (id == -1)
            return;

        auto it = points.find(id);
        if (it == points.end())
            return;

        it->second.second--;

        assert(it->second.second >= 0);
        if (it->second.second > 0)
            return;

        points.erase(it);
    }

    int add_ref(int id)
    {
        if (id == -1)
            return -1;

        auto it = points.find(id);
        if (it == points.end())
            throw std::runtime_error("Invalid id");
        it->second.second++;
        return id;
    }

    glm::vec2 &get(int id)
    {
        auto it = points.find(id);
        if (it == points.end())
            throw std::runtime_error("Invalid id");
        return it->second.first;
    }

    static control_points_t &get()
    {
        static control_points_t instance;
        return instance;
    }

    int get_closest_point(glm::vec2 pos) const
    {
        float min_dist = INFINITY;
        int closest = -1;

        for (auto &p : points)
        {
            float dist = glm::distance(p.second.first, pos);
            if (dist < min_dist)
            {
                min_dist = dist;
                closest = p.first;
            }
        }
        return closest;
    }

    void for_all(std::function<void(int, glm::vec2 &)> f)
    {
        for (auto &p : points)
            f(p.first, p.second.first);
    }

    bool is_valid(int id) const
    {
        return points.find(id) != points.end();
    }

private:
    // id -> pos + refcnt
    std::unordered_map<int, std::pair<glm::vec2, int>> points;
    int next_id = 0;
};

struct control_point
{
    const glm::vec2 &get() const { return control_points_t::get().get(id); }
    glm::vec2 &get() { return control_points_t::get().get(id); }
    operator glm::vec2() const { return get(); }

    control_point(const glm::vec2 &pos) : id(control_points_t::get().add_point(pos)) {}
    control_point(const control_point &other) : id(control_points_t::get().add_ref(other.id)) {}

    control_point &operator=(const glm::vec2 &other)
    {
        control_points_t::get().remove_point(id);
        id = control_points_t::get().add_point(other);
        return *this;
    }

    control_point &operator=(const control_point &other)
    {
        if (this == &other)
            return *this;
        control_points_t::get().remove_point(id);
        id = control_points_t::get().add_ref(other.id);
        return *this;
    }

    ~control_point()
    {
        control_points_t::get().remove_point(id);
    }

    int get_id() const { return id; }

    bool operator==(const control_point &cp) const { return id == cp.id; }
    bool operator!=(const control_point &cp) const { return id != cp.id; }

    static control_point invalid() { return control_point(-1); }
    static control_point from_id(int id) { return control_point(control_points_t::get().add_ref(id)); }

private:
    control_point(int id) : id(id) {}
    int id = -1;
};

/////////// Bezier /////////////////
using cubic_bezier = std::array<control_point, 4>;

///////////// APPLICATION //////////////
class App
{
public:
    App(GLFWwindow *window) : window(window)
    {
        glfwMakeContextCurrent(window);

        if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
        {
            std::printf("Failed to load OpenGL\n");
            exit(1);
        }

        ImGui::CreateContext();

        std::printf("OpenGL version: %s\n", glGetString(GL_VERSION));

        ImGui_ImplGlfw_InitForOpenGL(window, true);

        GLint major, minor;
        glGetIntegerv(GL_MAJOR_VERSION, &major);
        glGetIntegerv(GL_MINOR_VERSION, &minor);

        char glsl_version[128];
        if (major < 4 || (major == 4 && minor <= 6))
            std::sprintf(glsl_version, "#version %d%d0", major, minor);
        else
            std::strcpy(glsl_version, "#version 460");

        ImGui_ImplOpenGL3_Init(glsl_version);

        glEnable(GL_DEBUG_OUTPUT);
        glDebugMessageCallback([](GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar *message, const void *userParam)
                               { 
                                if(type == GL_DEBUG_TYPE_ERROR)
                                    std::fprintf(stderr, "GL CALLBACK: %s type = 0x%x, severity = 0x%x, message = %s\n",
                                              (type == GL_DEBUG_TYPE_ERROR ? "** GL ERROR **" : ""),
                                              type, severity, message); },
                               0);
    }

    ~App()
    {
        ImGui_ImplOpenGL3_Shutdown();
        ImGui_ImplGlfw_Shutdown();
        ImGui::DestroyContext();
        glfwDestroyWindow(window);
    }

    void run()
    {
        while (!glfwWindowShouldClose(window))
        {
            glfwPollEvents();

            ImGui_ImplOpenGL3_NewFrame();
            ImGui_ImplGlfw_NewFrame();

            ImGui::NewFrame();

            gui();

            ImGui::Render();

            draw();
            ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

            glfwSwapBuffers(window);
        }
    }

    virtual void gui() = 0;
    virtual void draw() = 0;

    GLFWwindow *window;
};

class MyApp : public App
{
public:
    using App::App;

    void gui() override
    {
        update_size();

        ImGui::Begin("Bezier");

        ImGui::InputInt("Subdivisions", &subdivision);

        auto Toggle = [](const char *msg, bool &b)
        {
            ImGui::Selectable(msg, &b);
            ImGui::SameLine();
            if (b)
                ImGui::TextColored(ImVec4(0.f, 1.f, 0.24f, 1.f), "ON");
            else
                ImGui::TextColored(ImVec4(1.f, 0.f, 0.f, 1.f), "OFF");
        };

        Toggle("Show lines", show_lines);
        Toggle("Show points", show_points);

        ImGui::InputText("File path: ", text, 1024);
        if (ImGui::Button("Save"))
            save_to_file(text);
        ImGui::SameLine();
        if (ImGui::Button("Load"))
            load_from_file(text);

        ImGui::End();

        if (show_points)
        {
            auto *drawList = ImGui::GetBackgroundDrawList();
            auto circleOnPoint = [&](int id, glm::vec2 &p)
            {
                if (id == selected_point.get_id())
                    drawList->AddCircleFilled(to_screen(p), 5.f, IM_COL32(255, 255, 0, 255));
                else
                    drawList->AddCircle(to_screen(p), 5.f, IM_COL32(255, 255, 0, 255));
            };
            control_points_t::get().for_all(circleOnPoint);
        }
    }

    void draw() override
    {
        ImGuiIO &io = ImGui::GetIO();
        if (!io.WantCaptureMouse)
            capture_mouse_inputs();

        if (!io.WantCaptureKeyboard)
            capture_keyboard_inputs();

        BezierShader::get().MVP = get_P();

        glClear(GL_COLOR_BUFFER_BIT);
        glDisable(GL_DEPTH_TEST);
        glDisable(GL_STENCIL_TEST);
        draw_beziers();
    }

    void capture_mouse_inputs()
    {
        ImGuiIO &io = ImGui::GetIO();
        glm::vec2 mouse_pos = from_screen(ImGui::GetMousePos());

        auto [closest, dist] = get_closest_point(mouse_pos);

        if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
            dragging_point = false;

        if (ImGui::IsMouseClicked(ImGuiMouseButton_Left))
            if (dist * scale < 6.f)
            {
                selected_point = closest;
                dragging_point = true;
            }

        if (ImGui::IsMouseDragging(ImGuiMouseButton_Left))
        {
            ImVec2 delta_im = ImGui::GetMouseDragDelta(ImGuiMouseButton_Left);
            ImGui::ResetMouseDragDelta(ImGuiMouseButton_Left);

            glm::vec2 delta = glm::vec2({delta_im.x, delta_im.y}) / scale;

            if (dragging_point)
            {
                selected_point.get() += delta;
                update_constraints(selected_point.get_id(), delta);
            }
            else
                center -= delta;

            ImGui::SetNextFrameWantCaptureMouse(false);
        }

        if (ImGui::IsMouseClicked(ImGuiMouseButton_Right))
        {
            control_point p_end =
                dist * scale < 6.f ? closest
                                   : control_point(mouse_pos);

            if (selected_point.get_id() != -1 &&
                get_node_of(selected_point) == selected_point &&
                get_node_of(p_end) == p_end &&
                selected_point != p_end)
            {

                add_bezier(selected_point, p_end);
                selected_point = p_end;
            }
        }

        if (io.MouseWheel != 0)
            scale *= powf(1.1f, io.MouseWheel);
    }

    void capture_keyboard_inputs()
    {
        ImGuiIO &io = ImGui::GetIO();
        glm::vec2 mouse_pos = from_screen(ImGui::GetMousePos());

        auto [closest, dist] = get_closest_point(mouse_pos);

        if (ImGui::IsKeyPressed(ImGuiKey::ImGuiKey_Q))
            glfwSetWindowShouldClose(window, true);

        if (ImGui::IsKeyPressed(ImGuiKey::ImGuiKey_Escape))
            if (!dragging_point)
                selected_point = control_point::invalid();

        if (ImGui::IsKeyPressed(ImGuiKey::ImGuiKey_N))
            selected_point = control_point(mouse_pos);

        if (ImGui::IsKeyPressed(ImGuiKey::ImGuiKey_Equal))
            scale *= 1.1f;

        if (ImGui::IsKeyPressed(ImGuiKey::ImGuiKey_Minus))
            scale /= 1.1f;

        // link
        if (ImGui::IsKeyPressed(ImGuiKey::ImGuiKey_L))
        {
            control_point node_c = get_node_of(closest);
            control_point node_s = get_node_of(selected_point);

            if (dist * scale < 6.f &&
                closest != selected_point &&
                node_c != closest &&
                node_s != selected_point &&
                node_c == node_s)
            {
                int c = node_c.get_id();
                int p1 = selected_point.get_id();
                int p2 = closest.get_id();

                if (p1 > p2)
                    std::swap(p1, p2);

                auto found = std::find(chains.begin(), chains.end(), std::tuple{c, p1, p2});

                if (found == chains.end())
                {
                    selected_point.get() = 2.f * node_c.get() - closest.get();
                    chains.push_back({c, p1, p2});
                }
            }
        }

        // unlink
        if (ImGui::IsKeyPressed(ImGuiKey_U))
        {
            control_point node_c = get_node_of(closest);
            control_point node_s = get_node_of(selected_point);

            if (dist * scale < 6.f &&
                closest != selected_point &&
                node_c != closest &&
                node_s != selected_point &&
                node_c == node_s)
            {
                int c = node_c.get_id();
                int p1 = selected_point.get_id();
                int p2 = closest.get_id();

                if (p1 > p2)
                    std::swap(p1, p2);

                chains.remove({c, p1, p2});
            }
        }

        if (ImGui::IsKeyPressed(ImGuiKey_D))
        {
            if (selected_point.get_id() != -1)
            {
                auto has_point = [&](const cubic_bezier &b)
                {
                    for (auto &c : b)
                        if (c == selected_point)
                            return true;
                    return false;
                };

                auto it = beziers.begin();
                while (it != beziers.end())
                {
                    it = std::find_if(it, beziers.end(), has_point);
                    if (it != beziers.end())
                        it = beziers.erase(it);
                }

                selected_point = control_point::invalid();
            }
        }
    }

private:
    bool show_lines = true;
    bool show_points = true;
    int subdivision = 100;

    std::list<cubic_bezier> beziers;             // bezier
    std::list<std::tuple<int, int, int>> chains; // id-id-id

    bool dragging_point = false;
    control_point selected_point = control_point::invalid();

    char text[1024];

    glm::vec2 center{0.f, 0.f};
    float scale = 1.0f;

    void update_size()
    {
        ImGuiIO &io = ImGui::GetIO();
        ImVec2 screen = io.DisplaySize;
        glViewport(0, 0, screen.x, screen.y);
    }

    control_point get_node_of(control_point cp)
    {
        int id = cp.get_id();

        for (auto &bezier : beziers)
        {
            if (bezier[0].get_id() == id ||
                bezier[1].get_id() == id)
                return bezier[0];

            if (bezier[2].get_id() == id ||
                bezier[3].get_id() == id)
                return bezier[3];
        }

        return cp;
    }

    std::pair<control_point, float> get_closest_point(glm::vec2 world)
    {
        control_point pt = control_point::from_id(control_points_t::get().get_closest_point(world));
        return {pt, pt.get_id() != -1 ? glm::distance(pt.get(), world) : INFINITY};
    }

    void draw_beziers()
    {
        for (auto &b : beziers)
        {
            glm::vec2 p0 = b[0];
            glm::vec2 p1 = b[1];
            glm::vec2 p2 = b[2];
            glm::vec2 p3 = b[3];

            auto color = IM_COL32(255, 255, 255, 255);

            BezierShader::get().draw(p0, p1, p2, p3, subdivision, color);

            if (show_lines)
            {
                auto *drawList = ImGui::GetBackgroundDrawList();
                drawList->AddLine(to_screen(p0), to_screen(p1), color);
                drawList->AddLine(to_screen(p2), to_screen(p3), color);
            }
        }
    }

    void add_bezier(control_point p0, control_point p3)
    {
        beziers.push_back({
            p0,
            control_point(p0.get() + (p3.get() - p0.get()) / 3.f),
            control_point(p0.get() + (p3.get() - p0.get()) * 2.f / 3.f),
            p3,
        });
    }

    void update_constraints(int id, glm::vec2 delta)
    {
        for (auto &bezier : beziers)
        {
            if (bezier[0].get_id() == id)
                bezier[1].get() += delta;
            if (bezier[3].get_id() == id)
                bezier[2].get() += delta;
        }

        for (auto &[common, p1, p2] : chains)
        {
            auto &pts = control_points_t::get();

            if (!pts.is_valid(common) ||
                !pts.is_valid(p1) ||
                !pts.is_valid(p2))
            {
                assert(false);
                continue;
            }

            if (p1 == id)
                pts.get(p2) -= delta;
            if (p2 == id)
                pts.get(p1) -= delta;
        }
    }

    glm::mat4 get_P() const
    {
        ImGuiIO &io = ImGui::GetIO();
        ImVec2 screen = io.DisplaySize;
        return glm::ortho(-screen.x / 2 * scale + center.x, screen.x / 2 * scale + center.x,
                          screen.y / 2 * scale + center.y, -screen.y / 2 * scale + center.y);
    }

    glm::vec2 from_screen(ImVec2 screen) const
    {
        ImVec2 screen_size = ImGui::GetIO().DisplaySize;
        return glm::vec2{screen.x - screen_size.x / 2.f, screen.y - screen_size.y / 2.f} / scale + center;
    }

    ImVec2 to_screen(glm::vec2 world) const
    {
        ImVec2 screen_size = ImGui::GetIO().DisplaySize;
        glm::vec2 screen = (world - center) * scale + glm::vec2{screen_size.x / 2.f, screen_size.y / 2.f};
        return {screen.x, screen.y};
    }

    void save_to_file(const char *filename) const
    {
        using namespace rapidjson;

        Document d;
        auto &allocator = d.GetAllocator();

        Value jnodes;
        Value jbeziers;
        Value jchains;

        jnodes.SetArray();
        jbeziers.SetArray();
        jchains.SetArray();

        std::unordered_map<int, int> id_map;
        int next_id = 0;

        auto add_to_id_map = [&](int id, glm::vec2 &pos)
        {
            auto [it, inserted] = id_map.insert({id, next_id++});
            assert(inserted);

            Value jnode;
            jnode.SetArray();
            jnode.PushBack(pos.x, allocator).PushBack(pos.y, allocator);

            jnodes.PushBack(jnode, allocator);
        };

        auto &cpts = control_points_t::get();

        cpts.for_all(add_to_id_map);

        for (const auto &b : beziers)
        {
            Value jbezier;
            jbezier.SetArray();
            for (const auto &p : b)
                jbezier.PushBack(id_map[p.get_id()], allocator);

            jbeziers.PushBack(jbezier, allocator);
        }

        for (const auto [c, p1, p2] : chains)
        {
            if (cpts.is_valid(c) && cpts.is_valid(p1) && cpts.is_valid(p2))
            {
                Value jchain;
                jchain.SetArray()
                    .PushBack(id_map[c], allocator)
                    .PushBack(id_map[p1], allocator)
                    .PushBack(id_map[p2], allocator);

                jchains.PushBack(
                    jchain,
                    allocator);
            }
        }

        d.SetObject();
        d.AddMember("nodes", jnodes, allocator);
        d.AddMember("beziers", jbeziers, allocator);
        d.AddMember("chains", jchains, allocator);

        std::ofstream f(filename);
        OStreamWrapper wrapper(f);
        PrettyWriter writer(wrapper);
        d.Accept(writer);
    }

    void load_from_file(const char *filename)
    {
        using namespace rapidjson;
        Document d;
        {
            std::ifstream f(filename);
            IStreamWrapper wrapper(f);
            d.ParseStream(wrapper);
        }

        if (d.HasParseError())
        {
            printf("%s\n", GetParseError_En(d.GetParseError()));
            return;
        }

        if (!d.IsObject() ||
            !d.HasMember("nodes") || !d.HasMember("beziers") || !d.HasMember("chains") ||
            !d["nodes"].IsArray() || !d["beziers"].IsArray() || !d["chains"].IsArray())
        {
            printf("invalid format\n");
            return;
        }

        beziers.clear();
        chains.clear();
        selected_point = control_point::invalid();
        auto &cpts = control_points_t::get();
        cpts.for_all([](int, glm::vec2 &)
                     { assert(false); });

        std::unordered_map<int, control_point> id_map;

        const Value &jnodes = d["nodes"];
        const Value &jbeziers = d["beziers"];
        const Value &jchains = d["chains"];

        for (int i = 0; i < jnodes.Size(); i++)
        {
            const Value &jnode = jnodes[i];

            control_point cp(glm::vec2{jnode[0].GetFloat(), jnode[1].GetFloat()});
            bool inserted = id_map.insert(std::pair{i, cp}).second;
            assert(inserted);
        }

        for (int i = 0; i < jbeziers.Size(); i++)
        {
            const Value &jbezier = jbeziers[i];
            beziers.push_back({id_map.at(jbezier[0].GetInt()),
                               id_map.at(jbezier[1].GetInt()),
                               id_map.at(jbezier[2].GetInt()),
                               id_map.at(jbezier[3].GetInt())});
        }

        for (int i = 0; i < jchains.Size(); i++)
        {
            const Value &jchain = jchains[i];
            chains.push_back({id_map.at(jchain[0].GetInt()).get_id(),
                              id_map.at(jchain[1].GetInt()).get_id(),
                              id_map.at(jchain[2].GetInt()).get_id()});
        }
    }
};

int main(int argc, char **argv)
{
    glfwInit();

    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_ANY_PROFILE);
    glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GLFW_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
    glfwWindowHint(GLFW_SAMPLES, 4);

    GLFWwindow *window = glfwCreateWindow(800, 600, "Bezier", NULL, NULL);
    if (!window)
    {
        const char *desc;
        glfwGetError(&desc);
        std::printf("Failed to create window\n%s\n", desc);
        return 1;
    }
    MyApp(window).run();

    glfwTerminate();
}
