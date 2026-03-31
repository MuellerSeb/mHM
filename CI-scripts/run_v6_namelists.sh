#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Run all v6 smoke namelists with one or more mHM v6 driver executables.

Usage:
  run_v6_namelists.sh [--log-dir DIR] [--threads N] [--mpi N] EXE [EXE ...]

Options:
  --log-dir DIR  Directory for per-run logs. Default: logs_v6
  --threads N    Set OMP_NUM_THREADS=N for each non-MPI run
  --mpi N        Run each executable via "mpirun -n N"
EOF
}

log_dir="logs_v6"
threads=0
mpi_ranks=0
executables=()

while (($# > 0)); do
  case "$1" in
    --log-dir)
      log_dir="$2"
      shift 2
      ;;
    --threads)
      threads="$2"
      shift 2
      ;;
    --mpi)
      mpi_ranks="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      executables+=("$@")
      break
      ;;
    -*)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
    *)
      executables+=("$1")
      shift
      ;;
  esac
done

if ((${#executables[@]} == 0)); then
  echo "At least one executable must be provided." >&2
  usage >&2
  exit 2
fi

if ((mpi_ranks > 0 && threads > 0)); then
  echo "Use either --mpi or --threads, not both." >&2
  exit 2
fi

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd "${script_dir}/.." && pwd)
cd "${repo_root}"

mkdir -p "${log_dir}"

cleanup_generated_outputs() {
  rm -f ./mpr*.nc ./mrm*.nc
}

run_namelist() {
  local exe="$1"
  local nml="$2"
  local exe_name nml_name log_path
  local -a cmd=()

  exe_name=$(basename "${exe}")
  nml_name=$(basename "${nml}" .nml)
  log_path="${log_dir}/${exe_name}__${nml_name}.log"

  if [[ "${nml_name}" == mpr_* ]]; then
    cmd+=("${exe}" -n "${nml}" -p mhm-para-template.nml -o mhm-output-template.nml)
  else
    cmd+=("${exe}" -n "${nml}" -o mhm-output-template.nml)
  fi

  if ((mpi_ranks > 0)); then
    cmd=(mpirun -n "${mpi_ranks}" "${cmd[@]}")
  fi

  echo "Running ${exe_name} with ${nml}"
  if ((threads > 0)); then
    if ! env OMP_NUM_THREADS="${threads}" "${cmd[@]}" >"${log_path}" 2>&1; then
      echo "Run failed: ${exe_name} ${nml}" >&2
      tail -n 50 "${log_path}" >&2 || true
      exit 1
    fi
  else
    if ! "${cmd[@]}" >"${log_path}" 2>&1; then
      echo "Run failed: ${exe_name} ${nml}" >&2
      tail -n 50 "${log_path}" >&2 || true
      exit 1
    fi
  fi

  echo "Completed ${exe_name} with ${nml}"
}

mapfile -t namelists < <(find test_nml -maxdepth 1 -type f -name '*.nml' | sort)

for exe in "${executables[@]}"; do
  if [[ ! -x "${exe}" ]]; then
    echo "Executable not found or not executable: ${exe}" >&2
    exit 1
  fi

  cleanup_generated_outputs
  for nml in "${namelists[@]}"; do
    run_namelist "${exe}" "${nml}"
  done
done
