{ lib
, buildPythonPackage
, fetchPypi
}:

buildPythonPackage rec {
  pname = "pytest-harvest";
  version = "1.10.4";

  src = fetchPypi {
    inherit pname version;
    sha256 = "+A1ylYw0lRHOtf+oolphZtHsarz93CNpBAjzG5ancSQ=";
  };

  nativeBuildInputs = with python3.pkgs; [
    setuptools-scm
  ];

  buildInputs = with python3.pkgs; [
    pytest
  ];

  propagatedBuildInputs = with python3.pkgs; [
    decopatch
    makefun
  ];

  checkInputs = with python3.pkgs; [
    pytestCheckHook
    pytest-cases
    pandas
    tabulate
  ];

  pythonImportsCheck = [
    "pytest_harvest"
  ];

  postPatch = ''
    # Not actually needed?
    substituteInPlace setup.cfg \
      --replace "pytest-runner" ""

    # Defining 'pytest_plugins' in a non-top-level conftest is no longer supported.
    # https://github.com/smarie/python-pytest-harvest/pull/62
    mv pytest_harvest/tests/conftest.py .
  '';

  meta = with lib; {
    description = "Library for creating step-wise/incremental tests in pytest";
    homepage = "https://github.com/smarie/python-pytest-harvest";
    license = licenses.bsd3;
    maintainers = with maintainers; [ ];
  };
}
