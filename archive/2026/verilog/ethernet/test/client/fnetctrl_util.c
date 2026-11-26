
/* Copyright (c) 2023, Haakan T. Johansson */
/* All rights reserved. */

/* Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the authors nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include "fnetctrl.h"

#include <stdio.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

/*************************************************************************/

typedef struct fnet_fork_info_t
{
  pid_t _child;
  int   _pipefd;
} fnet_fork_info;

fnet_fork_info _fnet_fork_info = { -1, -1 };

void fnet_fork_wait(void)
{
  /* Close the pipe goinf to the external process. */
  close(_fnet_fork_info._pipefd);
  /* And wait for it to finish. */
  waitpid(_fnet_fork_info._child, NULL, 0);
}

void fnet_fork_colouriser(int fd, const char *colouriser)
{
  char path[1024];
  char *rslash;
  struct stat sb;
  int pipefd[2];
  int successfd[2];
  pid_t child;
  int flags;
  char token = 1;

  /* Only if printing to a tty. */
  if (!isatty(fd))
    return;

  /* Construct the path to the colouriser. */

  readlink("/proc/self/exe", path, sizeof (path));
  path[sizeof (path)-1] = 0;

  /* printf ("self: %s\n", path); */

  rslash = strrchr(path, '/');

  if (rslash)
    strncpy(rslash+1, colouriser,
	    sizeof (path) - (rslash - path));
  else
    strncpy(path, colouriser,
	    sizeof (path));
  path[sizeof (path) - 1] = 0;

  /* printf ("path: %s\n", path); */

  /* See if the colouriser path seems to be available. */
  if (stat(path, &sb) != 0)
    return;

  /* And is executable, at least for someone.
   * Note: does not guarantee that it will work...
   */
  if (!(sb.st_mode & S_IXUSR))
    return;

  /* As we know it uses perl, that better be available too. */
  if (stat("/usr/bin/perl", &sb) != 0)
    return;

  /* And is executable, at least for someone.
   * Note: does not guarantee that it will work...
   */
  if (!(sb.st_mode & S_IXUSR))
    return;

  /* Make a pipe to pass our fd (stdout) to the forked process. */
  if (pipe(pipefd) != 0)
    return;
  /* pipefd[0] is for reading, pipefd[1] is for writing. */
  if (pipe(successfd) != 0)
    return;

  /* Prepare such that the success check pipe is closed-on-successful exec. */
  if ((flags = fcntl(successfd[1], F_GETFD)) == -1)
    return;
  if (fcntl(successfd[1], F_SETFD, flags | FD_CLOEXEC) == -1)
    return;

  child = fork();

  if (child == -1)
    return;

  if (child == 0)
    {
      /* We are the child. */

      if (dup2(pipefd[0], STDIN_FILENO) == -1)
	{
	  perror("dup");
	  exit(1);
	}

      close(pipefd[0]);
      close(pipefd[1]);
      close(successfd[0]);

      execl(/* "grr" to test failure */ path, path, NULL);

      /* Write our error message, ... */
      perror("exec");
      /* ... before telling the parent. */
      write(successfd[1], &token, 1);

      exit(1);
    }

  close (pipefd[0]);
  close (successfd[1]);

  /* Was the exec() successful in the child?
   *
   * If yes, then it have been closed.  If not, then there is a token
   * for us.
   */

  for ( ; ; )
    {
      ssize_t n;

      n = read(successfd[0], &token, 1);

      if (n > 0)
	{
	  /* We got a token, exec failed. */
	  fprintf (stderr,
		   "Failed to start colouriser '%s'- continuing normally.\n",
		   path);

	  close (successfd[0]);
	  close (pipefd[1]);
	  return;
	}

      if (n == 0)
	break;

      /* This typically should not happen with a pipe. */
      fprintf (stderr,
	       "Token read from colouriser failed, trying again.\n");
    }
  close (successfd[0]);

  /* We should write to the pipe. */
  if (dup2(pipefd[1], fd) == -1)
    return;

  /* In case dup2 failed, we could try to clean up the file handles.
   * But that is not critical, just means we have some lingering
   * file handles...
   */

  close (pipefd[1]);

  _fnet_fork_info._child  = child;
  _fnet_fork_info._pipefd = fd;

  atexit(fnet_fork_wait);
}
