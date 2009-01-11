(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* copied from freebsd-4 *)

INTERFACE Uerror;

CONST
  EPERM   = 1;
  ENOENT  = 2;
  ESRCH   = 3;
  EINTR   = 4;
  EIO     = 5;
  ENXIO   = 6;
  E2BIG   = 7;
  ENOEXEC = 8;
  EBADF   = 9;
  ECHILD  = 10;
  EDEADLK = 11;
  ENOMEM  = 12;
  EACCES  = 13;
  EFAULT  = 14;
  ENOTBLK = 15;
  EBUSY   = 16;
  EEXIST  = 17;
  EXDEV   = 18;
  ENODEV  = 19;
  ENOTDIR = 20;
  EISDIR  = 21;
  EINVAL  = 22;
  ENFILE  = 23;
  EMFILE  = 24;
  ENOTTY  = 25;
  ETXTBSY = 26;
  EFBIG   = 27;
  ENOSPC  = 28;
  ESPIPE  = 29;
  EROFS   = 30;
  EMLINK  = 31;
  EPIPE   = 32;
  EDOM    = 33;
  ERANGE  = 34;
  EAGAIN       = 35;
  EWOULDBLOCK  = EAGAIN;
  EINPROGRESS  = 36;
  EALREADY     = 37;
  ENOTSOCK        = 38;
  EDESTADDRREQ    = 39;
  EMSGSIZE        = 40;
  EPROTOTYPE      = 41;
  ENOPROTOOPT     = 42;
  EPROTONOSUPPORT = 43;
  ESOCKTNOSUPPORT = 44;
  EOPNOTSUPP      = 45;
  EPFNOSUPPORT    = 46;
  EAFNOSUPPORT    = 47;
  EADDRINUSE      = 48;
  EADDRNOTAVAIL   = 49;
  ENETDOWN        = 50;
  ENETUNREACH     = 51;
  ENETRESET       = 52;
  ECONNABORTED    = 53;
  ECONNRESET      = 54;
  ENOBUFS         = 55;
  EISCONN         = 56;
  ENOTCONN        = 57;
  ESHUTDOWN       = 58;
  ETOOMANYREFS    = 59;
  ETIMEDOUT       = 60;
  ECONNREFUSED    = 61;
  ELOOP           = 62;
  ENAMETOOLONG    = 63;
  EHOSTDOWN       = 64;
  EHOSTUNREACH    = 65;
  ENOTEMPTY       = 66;
  EPROCLIM        = 67;
  EUSERS          = 68;
  EDQUOT          = 69;
  ESTALE          = 70;
  EREMOTE         = 71;
  EBADRPC         = 72;
  ERPCMISMATCH    = 73;
  EPROGUNAVAIL    = 74;
  EPROGMISMATCH   = 75;
  EPROCUNAVAIL    = 76;
  ENOLCK          = 77;
  ENOSYS          = 78;
  EFTYPE          = 79;
  EAUTH           = 80;
  ENEEDAUTH       = 81;
  ELAST           = 81;

(* Extension by mjordan *)
CONST
  Max = ELAST;
  
END Uerror.
