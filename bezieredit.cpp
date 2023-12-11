#include <glad.h>

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <GLFW/glfw3.h>

#include <imgui.h>
#include <backends/imgui_impl_glfw.h>
#include <backends/imgui_impl_opengl3.h>

#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/istreamwrapper.h>
#include <rapidjson/ostreamwrapper.h>
#include <rapidjson/error/en.h>

#include <vector>
#include <unordered_map>
#include <map>
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

    void draw(const std::vector<glm::vec2> &control_points, int subdivision, ImU32 color)
    {
        use();

        glUniformMatrix4fv(0, 1, GL_FALSE, &MVP[0][0]);

        glUniform4f(6, (color & 0xff) / 255.0f,
                    ((color >> 8) & 0xff) / 255.0f,
                    ((color >> 16) & 0xff) / 255.0f,
                    ((color >> 24) & 0xff) / 255.0f);
        glUniform1i(4, subdivision);

        static_assert(sizeof(glm::vec2) == sizeof(float) * 2);
        assert(control_points.size() <= get_max_control_potins());

        glUniform1i(5, (GLint)control_points.size());
        glUniform2fv(7, (GLsizei)control_points.size(), &control_points[0][0]);

        glBindVertexArray(vao);
        glDrawArrays(GL_LINE_STRIP, 0, subdivision);
    }

    GLint get_max_control_potins() const
    {
        return 128;
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
layout(location = 4) uniform int subdivision;
layout(location = 5) uniform int control_points_count;
layout(location = 7) uniform vec2 control_points[128];

void main() {
    float t = float(gl_VertexID) / (subdivision - 1);
    const int n = control_points_count - 1;

    /*
    vec2 pos = vec2(0.0f, 0.0f);

    vec2 pts[control_points.length()];
    if (t < 0.5) {
        for(int i = 0; i <= n; i++)
            pts[i] = control_points[i];
    } else {
        t = 1 - t;
        for(int i = 0; i <= n; i++)
            pts[i] = control_points[n - i];
    }

    const float t_t1 = t / (1 - t);
    float mult = pow(1 - t, float(n));
    for(int i = 0; i <= n; i++)
    {
        pos += pts[i] * mult;
        mult *= t_t1 * float(n - i) / (i + 1);
    }
    */
    
    vec2 pos_s[control_points.length()];
    for(int i = 0; i < control_points_count; i++)
        pos_s[i] = control_points[i];
    
    for(int i = 0; i < n; i++)
    {
        for(int j = 0; j < n - i; j++)
            pos_s[j] = pos_s[j] * (1 - t) + pos_s[j + 1] * t;
    }

    vec2 pos = pos_s[0];

    gl_Position = MVP * vec4(pos, 0.0f, 1.0f);
})";

    static constexpr const char bezier_fragment_shader[] = R"(
#version 420
#extension GL_ARB_explicit_uniform_location : require

layout(location = 6) uniform vec4 color;
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
using bezier = std::vector<control_point>;

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

        ImGui::InputInt("Bezier degree", &bezier_degree);
        bezier_degree = std::max(1, std::min(BezierShader::get().get_max_control_potins() - 1, bezier_degree));

        ImGui::InputInt("Subdivisions", &subdivision);
        subdivision = std::max(1, std::min(100000, subdivision));

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

        if (ImGui::Button("Rotate"))
        {
            auto &cpts = control_points_t::get();
            cpts.for_all([&](int, glm::vec2 &p)
                         { glm::vec2 rel = p - center;
                            p = glm::vec2(rel.y, -rel.x) + center; });
        }

        if (ImGui::Button("Clear"))
            clear();

        ImGui::Text("Beziers: %zu", beziers.size());
        ImGui::Text("Chains: %zu", chains.size());

        if (ImGui::BeginTable("Bezier sizes", 2))
        {

            std::map<size_t, size_t> sizes;
            for (const auto &b : beziers)
            {
                sizes.insert({b.size(), 0});
                sizes[b.size()]++;
            }

            ImGui::TableSetupColumn("Size");
            ImGui::TableSetupColumn("Count");

            ImGui::TableHeadersRow();
            for (auto [size, count] : sizes)
            {
                ImGui::TableNextRow();
                ImGui::TableNextColumn();
                ImGui::Text("%zu", size);
                ImGui::TableNextColumn();
                ImGui::Text("%zu", count);
            }

            ImGui::EndTable();
        }

        ImGui::Text("FPS: %.1f", ImGui::GetIO().Framerate);

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
                if (selected_point != closest)
                    was_last_action_add_end_freestanding = false;

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
                update_constraints(selected_point, delta);
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

            if (ImGui::IsKeyDown(ImGuiKey_ModCtrl) && was_last_action_add_end_freestanding)
            {
                if (is_endpoint(p_end) &&
                    selected_point != p_end)
                {
                    bezier b = beziers.back();
                    assert(b.back() == selected_point);

                    if (b.size() < BezierShader::get().get_max_control_potins())
                    {
                        was_last_action_add_end_freestanding = is_freestanding(p_end);

                        b.push_back(p_end);
                        beziers.back() = b;
                        selected_point = p_end;
                    }
                }
            }
            else
            {
                if (is_endpoint(selected_point) &&
                    is_endpoint(p_end) &&
                    selected_point != p_end)
                {
                    was_last_action_add_end_freestanding = is_freestanding(p_end);

                    add_bezier(selected_point, p_end);
                    selected_point = p_end;
                }
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
            {
                selected_point = control_point::invalid();
                was_last_action_add_end_freestanding = false;
            }

        if (ImGui::IsKeyPressed(ImGuiKey::ImGuiKey_N))
        {
            selected_point = control_point(mouse_pos);
            was_last_action_add_end_freestanding = false;
        }

        if (ImGui::IsKeyPressed(ImGuiKey::ImGuiKey_Equal))
            scale *= 1.1f;

        if (ImGui::IsKeyPressed(ImGuiKey::ImGuiKey_Minus))
            scale /= 1.1f;

        // link
        if (ImGui::IsKeyPressed(ImGuiKey::ImGuiKey_L))
        {
            control_point node_c = get_node_of_derivative_cp(closest);
            control_point node_s = get_node_of_derivative_cp(selected_point);

            if (dist * scale < 6.f &&
                closest != selected_point &&
                node_c != closest &&
                node_s != selected_point &&
                node_c == node_s)
            {
                was_last_action_add_end_freestanding = false;

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
            control_point node_c = get_node_of_derivative_cp(closest);
            control_point node_s = get_node_of_derivative_cp(selected_point);

            if (dist * scale < 6.f &&
                closest != selected_point &&
                node_c != closest &&
                node_s != selected_point &&
                node_c == node_s)
            {
                was_last_action_add_end_freestanding = false;

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
                was_last_action_add_end_freestanding = false;

                auto has_point = [&](const bezier &b)
                {
                    for (auto &c : b)
                        if (c == selected_point)
                            return true;
                    return false;
                };

                beziers.remove_if(has_point);

                auto chain_contains_id = [id = selected_point.get_id()](const std::tuple<int, int, int> &t)
                {
                    auto [c, p1, p2] = t;
                    return c == id || p1 == id || p2 == id;
                };

                chains.remove_if(chain_contains_id);

                selected_point = control_point::invalid();
            }
        }
    }

private:
    bool show_lines = true;
    bool show_points = true;
    int subdivision = 1000;

    bool was_last_action_add_end_freestanding = false;

    int bezier_degree = 3;

    std::list<bezier> beziers;                   // bezier
    std::list<std::tuple<int, int, int>> chains; // id-id-id

    bool dragging_point = false;
    control_point selected_point = control_point::invalid();

    char text[1024] = "";

    glm::vec2 center{0.f, 0.f};
    float scale = 1.0f;

    void clear()
    {
        beziers.clear();
        chains.clear();
        selected_point = control_point::invalid();
        auto &cpts = control_points_t::get();
        cpts.for_all([](int, glm::vec2 &)
                     { assert(false); });

        dragging_point = false;
        center = {0.f, 0.f};
        scale = 1.0f;
    }

    void update_size()
    {
        ImGuiIO &io = ImGui::GetIO();
        ImVec2 screen = io.DisplaySize;
        glViewport(0, 0, screen.x, screen.y);
    }

    bool is_freestanding(control_point cp) const
    {
        if (cp == control_point::invalid())
            return false;

        // is freestanding point (not used in any bezier)
        return beziers.end() == std::find_if(beziers.begin(), beziers.end(), [&](const bezier &b)
                                             { return std::find(b.begin(), b.end(), cp) != b.end(); });
    }

    bool is_endpoint(control_point cp) const
    {
        if (cp == control_point::invalid())
            return false;

        // is endpoint of any bezier
        return beziers.end() != std::find_if(beziers.begin(), beziers.end(), [&](const bezier &b)
                                             { return b.front() == cp || b.back() == cp; }) ||
               is_freestanding(cp);
    }

    control_point get_node_of_derivative_cp(control_point cp) // for derivatives
    {
        auto b = std::find_if(beziers.begin(), beziers.end(), [&](const bezier &b)
                              { return b.size() >= 4 &&
                                       (b[1] == cp ||
                                        b[b.size() - 2] == cp); });

        if (b == beziers.end())
            return control_point::invalid();

        return (*b)[1] == cp ? b->front() : b->back();
    }

    std::pair<control_point, float> get_closest_point(glm::vec2 world)
    {
        control_point pt = control_point::from_id(control_points_t::get().get_closest_point(world));
        return {pt, pt.get_id() != -1 ? glm::distance(pt.get(), world) : INFINITY};
    }

    void draw_beziers()
    {
        for (const auto &b : beziers)
        {
            auto color = IM_COL32(255, 255, 255, 255);

            std::vector<glm::vec2> points(b.begin(), b.end());
            BezierShader::get().draw(points, subdivision, color);

            if (show_lines)
            {
                auto *drawList = ImGui::GetBackgroundDrawList();
                drawList->AddLine(to_screen(points[0]), to_screen(points[1]), color);
                drawList->AddLine(to_screen(points[points.size() - 2]), to_screen(points.back()), color);
            }
        }
    }

    void add_bezier(control_point start, control_point end)
    {
        bezier points;
        points.push_back(start);

        const glm::vec2 s_pos = start.get();
        const glm::vec2 e_pos = end.get();
        const glm::vec2 step = (e_pos - s_pos) / (float)bezier_degree;

        for (int i = 1; i < bezier_degree; i++)
            points.push_back(control_point(s_pos + step * (float)i));
        points.push_back(end);

        beziers.push_back(std::move(points));
    }

    void update_constraints(control_point id, glm::vec2 delta)
    {
        for (auto &bezier : beziers)
        {
            if (bezier.size() <= 3)
                continue;

            if (bezier.front() == id)
                bezier[1].get() += delta;
            if (bezier.back() == id)
                bezier[bezier.size() - 2].get() += delta;
        }

        for (auto [common, p1, p2] : chains)
        {
            auto &pts = control_points_t::get();

            if (!pts.is_valid(common) ||
                !pts.is_valid(p1) ||
                !pts.is_valid(p2))
            {
                continue;
            }

            control_point cp_common = control_point::from_id(common);
            control_point cp_p1 = control_point::from_id(p1);
            control_point cp_p2 = control_point::from_id(p2);

            if (cp_p1 == id)
                cp_p2.get() = 2.f * cp_common.get() - cp_p1.get();
            if (cp_p2 == id)
                cp_p1.get() = 2.f * cp_common.get() - cp_p2.get();
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

        Value jnodes(kArrayType);
        Value jbeziers(kArrayType);
        Value jchains(kArrayType);

        std::unordered_map<int, int> id_map;
        int next_id = 0;

        auto add_to_id_map = [&](int id, glm::vec2 &pos)
        {
            auto [it, inserted] = id_map.insert({id, next_id++});
            assert(inserted);

            Value jnode(kArrayType);
            jnode.PushBack(pos.x, allocator).PushBack(pos.y, allocator);

            jnodes.PushBack(jnode, allocator);
        };

        auto &cpts = control_points_t::get();

        cpts.for_all(add_to_id_map);

        for (const auto &b : beziers)
        {
            Value jbezier(kArrayType);
            for (const auto &p : b)
                jbezier.PushBack(id_map[p.get_id()], allocator);

            jbeziers.PushBack(jbezier, allocator);
        }

        for (const auto [c, p1, p2] : chains)
        {
            if (cpts.is_valid(c) && cpts.is_valid(p1) && cpts.is_valid(p2))
            {
                Value jchain(kArrayType);
                jchain
                    .PushBack(id_map[c], allocator)
                    .PushBack(id_map[p1], allocator)
                    .PushBack(id_map[p2], allocator);

                jchains.PushBack(
                    jchain,
                    allocator);
            }
        }

        d.SetObject()
            .AddMember("nodes", jnodes, allocator)
            .AddMember("beziers", jbeziers, allocator)
            .AddMember("chains", jchains, allocator);

        std::ofstream f(filename);
        OStreamWrapper wrapper(f);
        Writer writer(wrapper);
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

        clear();

        std::unordered_map<int, control_point> id_map;

        const Value &jnodes = d["nodes"];
        const Value &jbeziers = d["beziers"];
        const Value &jchains = d["chains"];

        for (int i = 0; i < jnodes.Size(); i++)
        {
            const Value &jnode = jnodes[i];

            if (!jnode.IsArray() || jnode.Size() != 2)
            {
                printf("invalid format\n");
                return;
            }

            control_point cp(glm::vec2{jnode[0].GetFloat(), jnode[1].GetFloat()});
            bool inserted = id_map.insert(std::pair{i, cp}).second;
            assert(inserted);
        }

        for (int i = 0; i < jbeziers.Size(); i++)
        {
            const Value &jbezier = jbeziers[i];
            if (!jbezier.IsArray() || jbezier.Size() < 2)
            {
                printf("invalid format\n");
                return;
            }

            bezier b;
            for (int j = 0; j < jbezier.Size(); j++)
            {
                int id = jbezier[j].GetInt();
                auto it = id_map.find(id);
                if (it == id_map.end())
                {
                    printf("invalid format\n");
                    return;
                }
                b.push_back(it->second);
            }
            beziers.push_back(std::move(b));
        }

        for (int i = 0; i < jchains.Size(); i++)
        {
            const Value &jchain = jchains[i];
            if (!jchain.IsArray() || jchain.Size() != 3)
            {
                printf("invalid format\n");
                return;
            }

            int c = jchain[0].GetInt();
            int p1 = jchain[1].GetInt();
            int p2 = jchain[2].GetInt();

            auto it_c = id_map.find(c);
            auto it_p1 = id_map.find(p1);
            auto it_p2 = id_map.find(p2);

            if (it_c == id_map.end() || it_p1 == id_map.end() || it_p2 == id_map.end())
            {
                printf("invalid format\n");
                return;
            }

            chains.push_back({it_c->second.get_id(), it_p1->second.get_id(), it_p2->second.get_id()});
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
