module QubesTopics where

data QVMColor = RED | ORANGE | YELLOW | GREEN | GRAY | BLUE | PURPLE | BLACK

data QVMType = Proxy | HVM | HVMTempl | Template | Net

data QVM a = 

class QubesVM vm where
    label :: vm -> QVMColor

