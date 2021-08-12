# Module to add dependencies that do not belong
# anywhere else

# UNIT TEST catch2

Include(FetchContent)

FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG        v2.13.1)

FetchContent_MakeAvailable(Catch2)

list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/contrib)
include(Catch)


# LOGGING spdlog

FetchContent_Declare(
  spdlog
  GIT_REPOSITORY  https://github.com/gabime/spdlog.git
  GIT_TAG        v1.9.1)

FetchContent_MakeAvailable(spdlog)
find_package(spdlog REQUIRED)

# PROPERTY TEST rapidcheck

FetchContent_Declare(
  rapidcheck
  GIT_REPOSITORY https://github.com/emil-e/rapidcheck.git)

FetchContent_MakeAvailable(rapidcheck)