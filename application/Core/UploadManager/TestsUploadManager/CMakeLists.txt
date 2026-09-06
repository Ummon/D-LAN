add_executable(TestsUploadManager Tests.cpp)
target_link_libraries(TestsUploadManager PRIVATE UploadManager PeerManager Qt6::Test)
add_test(NAME TestsUploadManager COMMAND TestsUploadManager)
