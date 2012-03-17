include_directories(${CMAKE_CURRENT_LIST_DIR})

macro(add_mosh_plugin nam)
    add_library(${nam} MODULE ${ARGN})
    set_target_properties(${nam} PROPERTIES
        FOLDER Plugins
        PREFIX ""
        SUFFIX ".mplg")
    install(TARGETS ${nam} DESTINATION plugins)
endmacro(add_mosh_plugin)

