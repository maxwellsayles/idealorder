#include <assert.h>
#include <sys/resource.h>

#include "attempts.h"
#include "ideal.h"
#include "difference.h"
#include "group_by_k.h"
#include "prime_powers.h"
#include "string_integer.h"
#include "util.h"

using namespace std;

int main(int argc, char** argv) {
  // Make sure the program doesn't go too crazy!
  struct rlimit l = {7*1024ULL*1024ULL*1024ULL, 7*1024ULL*1024ULL*1024ULL};
  setrlimit(RLIMIT_AS, &l);

  runPrimePowers();
  //  runDifference();
  //  runAttempts();

  return 0;
}

