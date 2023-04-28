
from copy import deepcopy as _clone
def clone(x): return _clone(x.v.p)

class built_in_list_(list):
    def _iter_(self): return iter(pointer_(value_(x)) for x in super().__iter__())
    def append(self, val): return super().append(val.v)
    def getattr(self, attr): return self.__getattribute__(attr)
    def setattr(self, name, val): return self.__setattr__(name, val)


class value_:
    def __init__(self, v):
        self.v = v

    def getattr(self, attr):
        if hasattr(self, attr):
            return self.__getattribute__(attr)
        if isinstance(self.v, (value_, pointer_)):
            return self.v.getattr(attr)
        return self.v.__getattribute__(attr)

    def setattr(self, name, val):
        if hasattr(self, name):
            return self.__setattr__(name, val)
        if isinstance(self.v, (value_, pointer_)):
            return self.v.setattr(name, val)
        return self.v.__setattr__(name, val)

    def _dereference_all(self):
        if hasattr(self.v, '_dereference_all'):
            return self.v._dereference_all()
        return self.v

    def __str__(self): return f'{self.v}'
    def __getitem__(self, pos): return self.v.__getitem__(pos)
    def __setitem__(self, pos, val): return self.v.__setitem__(pos, val)
    def __eq__(self, other): return self.v == other
    def __le__(self, other): return self.v <= other
    def __lt__(self, other): return self.v < other
    def __ge__(self, other): return self.v >= other
    def __gt__(self, other): return self.v > other

class pointer_:
    def __init__(self, p):
        self.p = p

    def getattr(self, attr):
        if hasattr(self, attr):
            return self.__getattribute__(attr)
        return self.p.getattr(attr)

    def setattr(self, name, val):  # todo only if is &mut
        if hasattr(self, name):
            return self.__setattr__(name, val)
        return self.p.setattr(name, val)

    def _dereference_all(self):
        if hasattr(self.p, '_dereference_all'):
            return self.p._dereference_all()
        return self.p

    def __str__(self): return f'&{self.p}'
    def __getitem__(self, pos): return self.p.__getitem__(pos)
    def __setitem__(self, pos, val): return self.p.__setitem__(pos, val)  # todo only if is &mut
    def __eq__(self, other): return self.p == other.p
    def __le__(self, other): return self.p <= other.p
    def __lt__(self, other): return self.p < other.p
    def __ge__(self, other): return self.p >= other.p
    def __gt__(self, other): return self.p > other.p
