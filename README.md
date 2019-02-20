tmux
----

  - `prefix` + <kbd>I</kbd>: Install plugins
  - `prefix` + <kbd>U</kbd>: Update plugins

pyenv
-----

    pyenv install -l               # List all downloadable Python versions
    pyenv install 3.7.0            # Download Python 3.7.0
    pyenv virtualenv 3.7.0 foobar  # Create a virtual env with Python 3.7.0
    pyenv local foobar             # Use 'foobar' virtual env in current dir
    pyenv global foobar            # Use 'foobar' env unless otherwise specified
    pyenv versions                 # List installed virtualenvs and Pythons
    pyenv version                  # Show current Python/virtualenv and why
