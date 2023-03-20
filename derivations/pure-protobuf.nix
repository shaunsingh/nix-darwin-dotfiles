{ lib
, buildPythonPackage
, fetchFromGitHub
, python3
,
}:
buildPythonPackage rec {
  pname = "pure-protobuf";
  version = "2.2.0";

  format = "pyproject";

  # PyPi lacks tests.
  src = fetchFromGitHub {
    owner = "eigenein";
    repo = "protobuf";
    rev = version;
    hash = "sha256-9yJlPZzQ/iQcDX2lsuRk4+cMRY7FhqNIcFhgF1FzdEI=";
  };

  nativeBuildInputs = with python3.pkgs; [
    hatchling
    hatch-vcs
  ];

  checkInputs = with python3.pkgs; [
    pytestCheckHook
    pytest-benchmark
  ];

  # pytest-benchmark currently broken.
  # https://github.com/ionelmc/pytest-benchmark/issues/226
  doCheck = false;

  SETUPTOOLS_SCM_PRETEND_VERSION = version;

  pythonImportsCheck = [
    "pure_protobuf"
  ];

  meta = with lib; {
    description = "Python implementation of Protocol Buffers with dataclass-based schemas";
    homepage = "https://github.com/eigenein/protobuf";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
