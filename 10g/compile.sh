fsharpc --nologo -a board.fs animal.fs moose.fs wolf.fs environment.fs -o "WolfMoose.dll" &&
fsharpc --nologo -r "WolfMoose.dll" run.fsx