This directory contains scripts that test various aspects of Eff:

- Folders `valid` and `invalid` contain basic regression tests, which ensure that
  inferred types and computed values are what we expect. Any time a bug is
  found, one should add a new test that covers it. Folder `valid` contains programs
  that terminate successfuly, while the ones in `invalid` must terminate with an error.

To add new test case to existing test folders:

- Create test file `text_xyz.eff` and reference file `text_xyz.eff.ref`.
- Run `dune runtest` to generate updated `dune.inc` file and `dune promote` to update it.

To create a new folder with tests:

- Create new folder somewhere under `tests`.
- Copy `dune` file from existing test folders and add empty `dune.inc` file. Fix path to `generate_dune_rules` and adjust configurations for new test generation. You can only configure expected exit code for now.
- Add test cases as above.