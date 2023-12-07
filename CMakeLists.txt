cmake_minimum_required(VERSION 3.5)
project(nifs3edit)

set(OpenGL_GL_PREFERENCE GLVND)
find_package(glfw3 REQUIRED)
find_package(glm REQUIRED)
find_package(OpenGL REQUIRED)

set(IMGUI_PATH ${CMAKE_CURRENT_SOURCE_DIR}/imgui)
file(GLOB IMGUI_SOURCES ${IMGUI_PATH}/*.cpp)

add_library(imgui ${IMGUI_SOURCES} ${IMGUI_PATH}/backends/imgui_impl_glfw.cpp
                  ${IMGUI_PATH}/backends/imgui_impl_opengl3.cpp)
target_include_directories(imgui PUBLIC ${IMGUI_PATH})
add_library(glad glad.c)
target_include_directories(glad PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(bezieredit bezieredit.cpp)
target_link_libraries(bezieredit glm::glm glfw glad imgui OpenGL::GL)
target_include_directories(bezieredit PRIVATE ${IMGUI_PATH} .)
target_compile_features(bezieredit PRIVATE cxx_std_17)
