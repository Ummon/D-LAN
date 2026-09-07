add_executable(TestsChatSystem Tests.cpp)
target_link_libraries(TestsChatSystem PRIVATE ChatSystem Qt6::Test)
add_test(NAME TestsChatSystem COMMAND TestsChatSystem)
