/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3unix.h"

void Uexec__RepackStatus(int* var_status)
{
    int status;

    assert(var_status != NULL);

    /* Posix says you must pass "the original" to the macros. */
    status = ((WTERMSIG(*var_status) << 8) | WEXITSTATUS(*var_status));
#ifdef WCOREDUMP
    status |= (WCOREDUMP(*var_status) ? 0x8000 : 0);
#endif
    *var_status = status;
}

/* If needed, define functions Uexec_WTERMSIG, Uexec_WEXITSTATUS, etc. */
