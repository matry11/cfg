# Organization Rationale
## "**symlinks-and-git-submodules**"-based way for managing all personal and custom environments in one place (so I don't have to go bananas organizing/versioning scripts anymore)
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
|       └── init.el
├── local (not versioned)
|   ├── bin
|   ├── src
│      ├── dist
│      └── plugins
│   └── tools
│      └── scripts
├── shared (not versioned)
|   ├── backup
│   └── misc
```
