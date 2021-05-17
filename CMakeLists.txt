project(catala)
cmake_minimum_required(VERSION 3.20)

add_custom_target(update_parser_messages
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    COMMAND dune build @update-parser-messages --auto-promote | true
)

add_custom_target(format
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    COMMAND make --no-print-directory format
)

add_custom_target(build_catala
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    COMMAND dune build ./src/catala/catala.exe
)

add_custom_target(build
    DEPENDS update_parser_messages format build_catala
)

add_custom_target(test_examples
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    COMMAND make --no-print-directory -C ./examples tests
    DEPENDS build
)

