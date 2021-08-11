# Module to add dependencies that do not belong
# anywhere else

# UNIT TEST with catch2

Include(FetchContent)

FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG        v2.13.1)

FetchContent_MakeAvailable(Catch2)

list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/contrib)
include(Catch)

FetchContent_Declare(
  spdlog
  GIT_REPOSITORY  https://github.com/gabime/spdlog.git
  GIT_TAG        v1.9.1)

FetchContent_MakeAvailable(spdlog)
find_package(spdlog REQUIRED)