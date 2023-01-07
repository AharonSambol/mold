def main():
    a: int = 12
    a: List[int] = [1, 2, a]
    s: List[List[int]] = [[1, 2, 3, 9], [1, 5]]
    b := s[0][0]
    for i in s.iter():
        dprint(i)

    print("--------")

    s.append([33, 3])
#     r := s.iter_mut()
#     r := reversed(s.iter())
#     ww := r.into_iter()
    for i in s.iter_mut():
        i.append(99)
        dprint(i)

