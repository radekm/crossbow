#define GIT_SHA1 "8c0f71670ced844e5ecc401660bc24a2588240d4"
static const char myversion[] = GIT_SHA1;
const char* get_git_version()
{
    return myversion;
}


