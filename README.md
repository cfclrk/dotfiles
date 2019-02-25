tmux
----

  - `prefix` + <kbd>I</kbd>: Install plugins
  - `prefix` + <kbd>U</kbd>: Update plugins

pyenv
-----

    pyenv install -l               # List all downloadable Python versions
    pyenv install 3.7.0            # Download and install Python 3.7.0
    pyenv virtualenv 3.7.0 foo     # Create a virtual env with Python 3.7.0
    pyenv local foo                # Use 'foo' virtual env in current dir
    pyenv global foo               # Use 'foo' env unless otherwise specified
    pyenv versions                 # List installed virtualenvs and Pythons
    pyenv version                  # Show current Python/virtualenv and why

rbenv
-----

    rbenv install -l               # List all downloadable Ruby versions
    rbenv install 2.6.1            # Download and install Ruby 2.6.1
    rbenv global 2.6.1             # Use Ruby 2.6.1 unless otherwise specified
