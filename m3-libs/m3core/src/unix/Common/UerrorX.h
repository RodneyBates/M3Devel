/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#ifndef _WIN32
/* These do exist but 
 - They have "WSA" in front of them.
 - They are large (10000+).
*/
X(EADDRINUSE)
X(EADDRNOTAVAIL)
X(EALREADY)
X(ECONNABORTED)
X(ECONNREFUSED)
X(ECONNRESET)
X(EHOSTDOWN)
X(EHOSTUNREACH)
X(EINPROGRESS)
X(EISCONN)
X(ENETDOWN)
X(ENETRESET)
X(ENETUNREACH)
X(ENOBUFS)
X(ENOTSOCK)
X(ETIMEDOUT)
X(EWOULDBLOCK)
#endif

X(EACCES)
X(EAGAIN)
X(EBADF)
X(ECHILD)
X(EDOM)
X(EEXIST)
X(EINTR)
X(EINVAL)
X(EIO)
X(EISDIR)
X(EMFILE)
X(ENAMETOOLONG)
X(ENFILE)
X(ENOENT)
X(ENOEXEC)
X(ENOMEM)
X(ENOTDIR)
X(ENOTEMPTY)
X(EPERM)
X(EPIPE)
X(ERANGE)

#ifdef E2BIG
X(E2BIG)
#endif
#ifdef EAFNOSUPPORT
X(EAFNOSUPPORT)
#endif
#ifdef EAUTH
X(EAUTH)
#endif
#ifdef EBADARCH
X(EBADARCH)
#endif
#ifdef EBADEXEC
X(EBADEXEC)
#endif
#ifdef EBADMACHO
X(EBADMACHO)
#endif
#ifdef EBADMSG
X(EBADMSG)
#endif
#ifdef EBADRPC
X(EBADRPC)
#endif
#ifdef EBUSY
X(EBUSY)
#endif
#ifdef ECANCELED
X(ECANCELED)
#endif
#ifdef EDEADLK
X(EDEADLK)
#endif
#ifdef EDESTADDRREQ
X(EDESTADDRREQ)
#endif
#ifdef EDEVERR
X(EDEVERR)
#endif
#ifdef EDQUOT
X(EDQUOT)
#endif
#ifdef EFAULT
X(EFAULT)
#endif
#ifdef EFBIG
X(EFBIG)
#endif
#ifdef EFTYPE
X(EFTYPE)
#endif
#ifdef EIDRM
X(EIDRM)
#endif
#ifdef EILSEQ
X(EILSEQ)
#endif
#ifdef ELOOP
X(ELOOP)
#endif
#ifdef EMLINK
X(EMLINK)
#endif
#ifdef EMSGSIZE
X(EMSGSIZE)
#endif
#ifdef EMULTIHOP
X(EMULTIHOP)
#endif
#ifdef ENEEDAUTH
X(ENEEDAUTH)
#endif
#ifdef ENOATTR
X(ENOATTR)
#endif
#ifdef ENODATA
X(ENODATA)
#endif
#ifdef ENODEV
X(ENODEV)
#endif
#ifdef ENOLCK
X(ENOLCK)
#endif
#ifdef ENOLINK
X(ENOLINK)
#endif
#ifdef ENOMSG
X(ENOMSG)
#endif
#ifdef ENOPOLICY
X(ENOPOLICY)
#endif
#ifdef ENOPROTOOPT
X(ENOPROTOOPT)
#endif
#ifdef ENOSPC
X(ENOSPC)
#endif
#ifdef ENOSR
X(ENOSR)
#endif
#ifdef ENOSTR
X(ENOSTR)
#endif
#ifdef ENOSYS
X(ENOSYS)
#endif
#ifdef ENOTBLK
X(ENOTBLK)
#endif
#ifdef ENOTCONN
X(ENOTCONN)
#endif
#ifdef ENOTSUP
X(ENOTSUP)
#endif
#ifdef ENOTTY
X(ENOTTY)
#endif
#ifdef ENXIO
X(ENXIO)
#endif
#ifdef EOPNOTSUPP
X(EOPNOTSUPP)
#endif
#ifdef EOVERFLOW
X(EOVERFLOW)
#endif
#ifdef EPFNOSUPPORT
X(EPFNOSUPPORT)
#endif
#ifdef EPROCLIM
X(EPROCLIM)
#endif
#ifdef EPROCUNAVAIL
X(EPROCUNAVAIL)
#endif
#ifdef EPROGMISMATCH
X(EPROGMISMATCH)
#endif
#ifdef EPROGUNAVAIL
X(EPROGUNAVAIL)
#endif
#ifdef EPROTO
X(EPROTO)
#endif
#ifdef EPROTONOSUPPORT
X(EPROTONOSUPPORT)
#endif
#ifdef EPROTOTYPE
X(EPROTOTYPE)
#endif
#ifdef EPWROFF
X(EPWROFF)
#endif
#ifdef EREMOTE
X(EREMOTE)
#endif
#ifdef EROFS
X(EROFS)
#endif
#ifdef ERPCMISMATCH
X(ERPCMISMATCH)
#endif
#ifdef ESHLIBVERS
X(ESHLIBVERS)
#endif
#ifdef ESHUTDOWN
X(ESHUTDOWN)
#endif
#ifdef ESOCKTNOSUPPORT
X(ESOCKTNOSUPPORT)
#endif
#ifdef ESPIPE
X(ESPIPE)
#endif
#ifdef ESRCH
X(ESRCH)
#endif
#ifdef ESTALE
X(ESTALE)
#endif
#ifdef ETIME
X(ETIME)
#endif
#ifdef ETOOMANYREFS
X(ETOOMANYREFS)
#endif
#ifdef ETXTBSY
X(ETXTBSY)
#endif
#ifdef EUSERS
X(EUSERS)
#endif
#ifdef EXDEV
X(EXDEV)
#endif
