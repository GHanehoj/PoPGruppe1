Biblioteket kan kompileres ved hjælp af fsharp kompileren, ved at køre kommandoen
fsharpc --nologo -a awariLib.fs

Herefter  kan  spillet  spilles  ved  at køre
fsharpc awariApp.fsx && mono awariApp.exe

Biblioteket kan testes ved  at  køre  white-box  testen  med  kommandoen
fsharpc awariLibTest.fsx && mono awariLibTest.exe

Læg mærke til, at det ikke er nødvendigt at importere biblioteket gennem kommandolinjen, da dette gøres i applikationerne.
