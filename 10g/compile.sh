fsharpc --nologo -a board.fs animal.fs moose.fs wolf.fs environment.fs -o "WolfMoose.dll" &&
fsharpc --nologo -r "WolfMoose.dll" run.fsx &&
mono run.exe 40 test.txt 10 30 10 2 10 8
