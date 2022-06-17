// Instal prolog using MAC
brew install gnu-prolog


// Use the following command to start prolog.
gprolog

// Load au color map
consult('au_map_color_sol.pl').

// Clear memory
abolish(counter/1).