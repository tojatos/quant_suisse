
w1 = "anagram"
w2 = "margana"
w3 = "ananas"

def is_anagram(s1, s2):

    if len(s1) != len(s2):
        return False

    s1_dict = {}

    for x in s1:
        if x not in s1_dict:
            s1_dict[x] = 1
        else:
            s1_dict[x] += 1
    for x in s2:
        if x not in s1_dict:
            return False
        if s1_dict[x] == 0:
            return False
        s1_dict[x] -= 1
    return True

print(is_anagram(w1, w2))
print(is_anagram(w1, w3))
