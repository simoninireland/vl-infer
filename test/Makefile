# Build the test files
#
# Copyright (C) 2026 Simon Dobson
#
# This file is part of vl-infer, a machine learning inference accelerator builder
#
# vl-infer is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# vl-infer is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

TEST_SCHEMATA = \
	eclectic.fbs \
	montsers.fbs
TEST_FLATBUFFERS = \
	eclectic-example.fb \
	monsters-example.fb
TEST_GENERATORS = \
	generate-eclectic.py \
	generate-monsters.py
TEST_MODULES = \
	Eclectic \
	MyGame

FLATC = flatc --python
PYTHON = python
RM = rm -fr


tests: $(TEST_FLATBUFFERS)


eclectic-example.fb:
	$(FLATC) eclectic.fbs
	$(PYTHON) generate-eclectic.py

monsters-example.fb:
	$(FLATC) monsters.fbs
	$(PYTHON) generate-monsters.py


clean:
	$(RM) $(TEST_FLATBUFFERS) $(TEST_MODULES)
