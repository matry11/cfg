# Organization Rationale
## "**symlinks-and-git-submodules**"-based way for managing all personal configs and custom environments in one place (without script glue)
____
```
.
├── meta
│   ├── switch-profile.el
│   ├── snapshots (not versioned)
│   └── profiles (only scripts are versioned)
│       ├── default   # current gui (emacs-exwm)
│       ├── emulators # standalone emu collection
│       ├── linux     # linux-based toolkits
│       ├── hyper-v   # staging environments 
|       └── windows   # flarevm, darksurgeon 
├── ui
│   ├── colors
│   ├── fonts
|   └── terminals
|      ├── backgrounds (not versioned)
|      ├── st
|      ├── xterm
|      └── lxterm
│   └── emacs
|      ├── lisp 	  # functions, hook-functions, plugins ..
|      └── init.el
├── local (not versioned)
|   ├── bin           # binaries/apps i use everyday
|   ├── src           # source-code of my own
│      ├── dist       # source-code of my own (builds)
│      └── plugins    # third-party plugins
│   └── tools
│      └── scripts    # scripts i use everyday
├── shared (not versioned)
|   ├── backup        # archived version of "my desktop"
│   └── misc          # the live version of "my desktop"
```
