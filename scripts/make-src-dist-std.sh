#!/bin/sh

if [ -n "$ROOT" -a -d "$ROOT" ] ; then
  sysinfo="$ROOT/scripts/sysinfo.sh"
  root="${ROOT}"; export root
else
  root=`pwd`
  while [ -n "$root" -a ! -f "$root/scripts/sysinfo.sh" ] ; do
    root=`dirname $root`
  done
  sysinfo="$root/scripts/sysinfo.sh"
  if [ ! -f "$sysinfo" ] ; then
    echo "scripts/sysinfo.sh not found" 1>&2
    exit 1
  fi
  export root
fi
. "$sysinfo"
# if a datestamp is set for the build of snapshots, include this, too
if [ -n "$DS" ]; then
  CM3VERSION="${CM3VERSION}-${DS}"
fi
. "$ROOT/scripts/pkginfo.sh"
. "$ROOT/scripts/pkgcmds.sh"

STAGE="${STAGE:-${TMPDIR}}"
[ -d "${STAGE}" ] || mkdir "${STAGE}" || mkdir -p "${STAGE}" || exit 1
ARCHIVE="${STAGE}/cm3-src-std-${CM3VERSION}.tgz"
header "building CM3 source distribution in ${ARCHIVE}"

#-----------------------------------------------------------------------------
# build the source distribution archive
#
CM3_ALL=yes
P=`fgrep " std" $ROOT/scripts/pkginfo.txt | awk "{print \\$1}" | tr '\\n' ' '`

PKGS=`pkgpath ${P}`
#echo ${P}
#echo ${PKGS}
cd "${ROOT}" || exit 1
/bin/ls -1d COPYRIGHT-CMASS COPYRIGHT-DEC scripts ${PKGS} > .tar-include
/bin/ls -1d m3overrides >> .tar-include
/bin/ls -1d m3-*/*/${TARGET} > .tar-exclude
/bin/ls -1d m3-*/*/${TARGET}p >> .tar-exclude
echo "building exclude list..."
$FIND . \( -name '*~' -or -name '*.bak' -or -name '*.orig' -or \
          -name '*.rej'  -or -name 'cvs-nq-up' -or -name '*-diffs' -or \
          -name 'PkgDep' -or -name 'PkgKind' -or -name '.bok' -or \
          -name '*.o' -or -name '*.a' -or -name '*.dll' -or -name '*.obj' -or \
          -name '.errors' -or -name '*.io' -or -name '*.mo' -or \
          -name '*.so' -or -name '*.so.[0-9]*' -or -name '.M3WEB' -or \
          -name '.M3SHIP' -or -name '.M3IMPTAB' -or -name '.M3EXPORTS' -or \
          \( -name 'CVS' -a -type d \) \) -print | \
  sed -e 's;^./;;' >> .tar-exclude

TARGETS=`cd ${ROOT}/m3-sys/cm3/src/config; echo *`

for p in ${PKGS}; do
  for t in ${TARGETS}; do
    echo ${p}/${t} >> .tar-exclude
  done
done

echo "archiving..."
export GZIP="-9 -v"
${TAR} -czf ${ARCHIVE} --files-from .tar-include --exclude-from .tar-exclude \
 || exit 1
ls -l ${ARCHIVE}
if [ -n "${DOSHIP}" ]; then
  if test "x${CM3CVSUSER}" != "x"; then
    CM3CVSUSER_AT="${CM3CVSUSER}@"
  else
    CM3CVSUSER_AT=""
  fi
  WWWSERVER=${WWWSERVER:-${CM3CVSUSER_AT}birch.elegosoft.com}
  WWWDEST=${WWWDEST:-${WWWSERVER}:/var/www/modula3.elegosoft.com/cm3/snaps}
  scp "${ARCHIVE}" "${WWWDEST}" < /dev/null
fi
echo "done"
exit 0
