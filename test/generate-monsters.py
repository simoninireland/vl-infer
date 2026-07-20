#!/bin/env python

# Script to generate a flatbuffer according to the monsters.fbs schema
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

import flatbuffers
import MyGame.Sample.Color
import MyGame.Sample.Equipment
import MyGame.Sample.Monster
import MyGame.Sample.Vec3
import MyGame.Sample.Weapon

builder = flatbuffers.Builder(1024)

#weapon_one = builder.CreateString('Sword')
#weapon_two = builder.CreateString('Axe')

# Create the first `Weapon` ('Sword').
#MyGame.Sample.Weapon.Start(builder)
#MyGame.Sample.Weapon.AddName(builder, weapon_one)
#MyGame.Sample.Weapon.AddDamage(builder, 3)
#sword = MyGame.Sample.Weapon.End(builder)

# Create the second `Weapon` ('Axe').
#MyGame.Sample.Weapon.Start(builder)
#MyGame.Sample.Weapon.AddName(builder, weapon_two)
#MyGame.Sample.Weapon.AddDamage(builder, 5)
#axe = MyGame.Sample.Weapon.End(builder)

# Serialize a name for our monster, called "Orc".
name = builder.CreateString("Orc")

# Create a `vector` representing the inventory of the Orc. Each number
# could correspond to an item that can be claimed after he is slain.
# Note: Since we prepend the bytes, this loop iterates in reverse.
#MyGame.Sample.Monster.StartInventoryVector(builder, 10)
#for i in reversed(range(0, 10)):
#  builder.PrependByte(i)
#inv = builder.EndVector()

# Create a FlatBuffer vector and prepend the weapons.
# Note: Since we prepend the data, prepend them in reverse order.
#MyGame.Sample.Monster.StartWeaponsVector(builder, 2)
#builder.PrependUOffsetTRelative(axe)
#builder.PrependUOffsetTRelative(sword)
#weapons = builder.EndVector()

# Path as a sequence of positions
#MyGame.Sample.Monster.StartPathVector(builder, 2)
#MyGame.Sample.Vec3.CreateVec3(builder, 1.0, 2.0, 3.0)
#MyGame.Sample.Vec3.CreateVec3(builder, 4.0, 5.0, 6.0)
#path = builder.EndVector()

# Create our monster by using `Monster.Start()` and `Monster.End()`.
MyGame.Sample.Monster.Start(builder)
MyGame.Sample.Monster.AddPos(builder,
                        MyGame.Sample.Vec3.CreateVec3(builder, 1, 2 , 3))
MyGame.Sample.Monster.AddMana(builder, 300)
MyGame.Sample.Monster.AddHp(builder, 300)
MyGame.Sample.Monster.AddName(builder, name)
#MyGame.Sample.Monster.AddInventory(builder, inv)
#MyGame.Sample.Monster.AddColor(builder, MyGame.Sample.Color.Color().Red)
#MyGame.Sample.Monster.AddWeapons(builder, weapons)
#MyGame.Sample.Monster.AddEquippedType(builder, MyGame.Sample.Equipment.Equipment().Weapon)
#MyGame.Sample.Monster.AddEquipped(builder, axe)
#MyGame.Sample.Monster.AddPath(builder, path)
orc = MyGame.Sample.Monster.End(builder)

# Call `Finish()` to instruct the builder that this monster is complete.
builder.Finish(orc)

buf = builder.Output()
with open("monsters-example.fb", "wb") as f:
    f.write(buf)
