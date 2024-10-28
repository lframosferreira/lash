#include "lash.h"
#include "lash-diag.h"

int main()
{
  if (lash_init() < 0)
    lash_perror("lash_init");

  /* ... call package functions ... */

  if (lash_end() < 0)
    lash_perror("lash_end");

  printf("### statistics ###\n");
  printf("residual memory : %llu byte(s).\n", lash_get_mem_usage());
  printf("max memory      : %llu byte(s).\n", lash_get_max_mem_usage());
  printf("### end of the test ###\n");   

  exit(0);
}
