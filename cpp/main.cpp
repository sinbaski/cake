#include <RInside.h>

using namespace std;

int main(int argc, char *argv[])
{
    RInside R(argc, argv);
    R["txt"] = "Hello World!\n";
    R.parseEvalQ("cat(txt)");
    return 0;
}

