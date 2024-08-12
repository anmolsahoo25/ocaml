#define CAML_INTERNALS

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include "caml/alloc.h"
#include "caml/memory.h"

CAMLprim value caml_runz3(value v) {
  CAMLparam1(v);
  CAMLlocal1(ret);

  int fds0[2];
  int fds1[2];

  const char* query = String_val(v);

  if(pipe(fds0) == -1) {
    printf("could not create pipe\n");
  }
  
  if(pipe(fds1) == -1) {
    printf("could not create pipe\n");
  }

  int returns = 1;
  int pid = fork();

  if (pid == 0) {
    caml_atfork_hook();
    // receive stdin on pipe
    close(fds0[1]);
    dup2(fds0[0], STDIN_FILENO);
    close(fds0[0]);

    // redirect stdout to pipe 
    close(fds1[0]);
    dup2(fds1[1], STDOUT_FILENO);
    close(fds1[1]);

    // run command
    char* args[] = { "z3", "-in", "-model", NULL };
    execvp("z3", args);
  }

  write(fds0[1], query, strlen(query)-1);
  close(fds0[1]);

  char result[1024];
  int read_value = read(fds1[0], result, 1024);
  char* final = malloc(sizeof(char) * (read_value - 1));
  for(int i = 0; i < read_value-1; i++) {
    final[i] = result[i];
  }
  waitpid(pid, &returns, 0);

  ret = caml_alloc_initialized_string(read_value-1, final);
  CAMLreturn(ret);
}
  /*
  const char* filename = String_val(v);
  char* command[2048];
  sprintf(command, "'/usr/local/bin/z3 %s'", filename);
  printf("%s\n", command);

  FILE *p;
  char res[2048];
  p = popen(command, "r");
  if (p != NULL) {
    fscanf(p, "%s", &res);
    printf("%s\n", res);
  }
  printf("return: %d\n", pclose(p));

}
*/
