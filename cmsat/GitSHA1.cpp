#define GIT_SHA1 "7fa2162dffb517f58264ec6deb45bcb7a617b199"
static const char myversion[] = GIT_SHA1;
const char* get_git_version()
{
    return myversion;
}


