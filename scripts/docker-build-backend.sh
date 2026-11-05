#!/bin/sh
# These directories are locked BuildKit caches, shared by dependency and app builds.
set -eu

work_dir="${MARVEL_BUILD_WORK_DIR:-/build/.stack-work}"
dirty="$work_dir/.docker-build-in-progress"
complete="$work_dir/.docker-build-complete"
mkdir -p "$work_dir"
set --

remove_executable() {
  for executable in "$work_dir"/dist/*/ghc-*/build/marvel-champions; do
    [ ! -d "$executable" ] || rm -rf "$executable"
  done
}

if [ -f "$dirty" ]; then
  echo "Recovering compilation artifacts from an interrupted Docker build"
  # An interrupted write can leave a nonempty, truncated object beside a valid
  # interface. Rebuild artifacts touched by that run, preserving the older cache.
  find "$work_dir" -type f -newer "$dirty" \
    \( -name '*.o' -o -name '*.hi' -o -name '*.dyn_o' -o -name '*.dyn_hi' \
       -o -name '*.o-boot' -o -name '*.hi-boot' \
       -o -name '*.dyn_o-boot' -o -name '*.dyn_hi-boot' \
       -o -name '*.a' -o -name '*.so' -o -name '*.tmp' \) -exec rm -f {} +
  remove_executable
  # Stack must invoke GHC even if it saved its package cache before interruption.
  set -- --force-dirty
fi

rm -f "$complete"
touch "$dirty"
if [ "${MARVEL_BUILD_TIMINGS:-0}" = 1 ]; then
  set -- "$@" --ghc-options "-ddump-timings +RTS -s -RTS"
fi
# Stack's ticker emits EINTR errors under Rosetta. This flag affects Stack, not
# the compiler or the server's runtime settings.
if stack +RTS -V0 -RTS install --no-terminal "$@" --local-bin-path /out MarvelChampions:exe:marvel-champions; then
  touch "$complete"
  rm "$dirty"
else
  status=$?
  # A normal compiler failure returns control, so preserve modules that finished.
  # An interface written without its object (or with an older/empty object) must
  # be rebuilt. Only dist has paired objects; installed interfaces use archives.
  if [ -d "$work_dir/dist" ]; then
    find "$work_dir/dist" -type f \( -name '*.hi' -o -name '*.dyn_hi' \) -print |
      while IFS= read -r interface; do
        case "$interface" in
          *.dyn_hi) stem="${interface%.dyn_hi}"; object="$stem.dyn_o" ;;
          *.hi) stem="${interface%.hi}"; object="$stem.o" ;;
        esac
        if [ ! -s "$interface" ] || [ ! -s "$object" ] || [ "$interface" -nt "$object" ]; then
          rm -f "$stem.hi" "$stem.o" "$stem.dyn_hi" "$stem.dyn_o"
        fi
      done
  fi
  find "$work_dir" -type f -newer "$dirty" \
    \( -name '*.a' -o -name '*.so' -o -name '*.tmp' \) -exec rm -f {} +
  remove_executable
  # Advance the recovery checkpoint only after the compiler exited and its
  # incomplete outputs were removed. A kill during compilation never gets here.
  touch "$dirty"
  exit "$status"
fi
