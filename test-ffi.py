# from ctypes import *

# lib = windll.mylib

# api = {
    
# # func name restype argtypes
#     'adder' : (c_int, [c_int, c_int]),
#     'subtractor': (c_float, [c_float, c_float]),
#     'factorial' : (c_int, [c_int]),
#     'hello' : (c_char_p, [c_char_p]),
#     'mystring' : (c_char_p, []),
# }
        

# for func in api:
#     f = getattr(lib, func)
#     f.restype, f.argtypes = api[func]
#     globals()[func] = f
    
# print adder(1,2)
# print subtractor(10.5, 2.5)
# print factorial(10)
# print hello('hello')
# print mystring()
from ctypes import *
from ctypes.util import find_library

import struct
import msgpack

libc = cdll.msvcrt

def make_msgpack_fun(fun):
    fun.restype = POINTER(c_char)

    def f(*args):
        packed = msgpack.packb(args)
        length_64bits = struct.pack(">q", len(packed)) # big-endian
        ptr = fun(length_64bits + packed)
        data_length = struct.unpack(">q", ptr[:8])[0]
        res = msgpack.unpackb(ptr[8:8+data_length])
        libc.free(ptr)
        return res

    return f


hsDLL = cdll.LoadLibrary("C:/Users/Bence/Desktop/bullethell/bullethell-new-stack/bullethell/bullethell.dll")
# hello = libc.hello_export
hello = make_msgpack_fun(hsDLL.hello_export)
adder = hsDLL.adder
# libc.getRandomNoState_export.restype = float
getRand = make_msgpack_fun(hsDLL.getRandomNoState_export)
loadPicTest = make_msgpack_fun(hsDLL.loadPicTest_export)
gameIterators = make_msgpack_fun(hsDLL.gameIterators_export)
testGameState = make_msgpack_fun(hsDLL.testGameState_export)
pythonInputHandler = make_msgpack_fun(hsDLL.pythonInputHandler_export)
hsDLL.hs_init()
print(hello("asd"))
print(adder(1,3))
print(getRand(2,5))
# print(getRand(c_float(2), c_float(5)))
print(loadPicTest("C:/Users/Bence/Desktop/bullethell-new-stack/bullethell/app/sprites/swole-doge.png"))
print(testGameState())
print(gameIterators(0.1, gameIterators(0.1, gameIterators(0.1, testGameState()))))
print(pythonInputHandler(1, gameIterators(0.1, testGameState())))
# print(hello("asd".encode("ascii")))




class V2:
    x = 0.0
    y = 0.0

    def __init__(self, basicsIn):
        self.x = basicsIn[0]
        self.y = basicsIn[1]

    def toBasics(self):
        return self.x, self.y


class Entity:
    entityID = 0
    entityClass = "asd"
    location = V2((0.0, 0.0))
    size = V2((0.0, 0.0))
    velocity = V2((0.0, 0.0))

    def __init__(self, basicsIn):
        self.entityID = basicsIn[0]
        self.entityClass = basicsIn[1]
        self.location = V2(basicsIn[2])
        self.size = V2(basicsIn[3])
        self.velocity = V2(basicsIn[4])

    def toBasics(self):
        return self.entityID, self.entityClass, self.location.toBasics(), self.size.toBasics(), self.velocity.toBasics()

# data Entity = Entity
#     { entityID    :: Int
#     , entityClass :: EntityClass
#     , location    :: V2
#     , size        :: V2
#     , velocity    :: V2
#     , sprite      :: Picture
#     }
#     | NullEntity


class GameState:
    def __init__(self, basicsIn):
        self.dimensions = V2(basicsIn[0])
        self.worldPos = V2(basicsIn[1])
        self.entities = [Entity(e) for e in basicsIn[2]]
        self.particles = [Entity(p) for p in basicsIn[3]]
        self.states = basicsIn[4]
        self.highestEntityId = basicsIn[5]
        self.randomSeed = basicsIn[6][0]
        self.idOfNextRandom = basicsIn[6][1]
        self.time = basicsIn[7][0]
        self.prevGenTime = basicsIn[7][1]
        self.hits = basicsIn[8]

    @classmethod
    def initFromNothing(cls):
        cls.dimensions = V2((0.0, 0.0))
        cls.worldPos = V2((0.0, 0.0))
        cls.entities = []
        cls.particles = []
        cls.states = [True]
        cls.highestEntityId = 0
        cls.randomSeed = 0
        cls.idOfNextRandom = 0
        cls.time = 0.0
        cls.prevGenTime = 0.0
        cls.hits = 0
        return cls

    def toBasics(self):
        return (self.dimensions.toBasics(),
                self.worldPos.toBasics(),
                [e.toBasics() for e in self.entities],
                [p.toBasics() for p in self.particles],
                [b for b in self.states],
                self.highestEntityId,
                (self.randomSeed, self.idOfNextRandom),
                (self.time, self.prevGenTime),
                self.hits)

print(GameState.initFromNothing().dimensions.x)







class Player:
    def __init__(self, initState):
        self.currentState = initState

    def makeMove(self):
        if self.currentState.entities[0].velocity.y == 0.0:
            return 3
        else:
            return 0




class Runner:
    def __init__(self, player):
        self.player = player
    
    def iterate(self):
        move = self.player.makeMove()
        inputtedStateBasics = pythonInputHandler(move, self.player.currentState.toBasics())
        newState = GameState(gameIterators(0.01667, inputtedStateBasics))
        self.player.currentState = newState
        print(newState.toBasics())
        return self




testPlayer = Player(GameState(testGameState()))
testRunner = Runner(testPlayer)
print(GameState(testGameState()).toBasics())
# testRunner.iterate()
print(testRunner.iterate().iterate().iterate().iterate().player.currentState.toBasics())




# -- GameState basics:

# -- ((Float, Float),
# --  (Float, Float),
# --  [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
# --  [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
# --  [Bool],
# --  Int,
# --  Int,
# --  Int,
# --  Float,
# --  Float,
# --  Int)









# starting state
# input - inputHandlerIO
# iterate - gameIteratorsIO
# communicate - stateToPicIO, etc