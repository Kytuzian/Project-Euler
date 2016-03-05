from sage.all import *

def run():
    g = NumberList(lambda n: n * (n + 1) / 2)

    def get_word_score(s):
        return sum([ord(a) - 64 for a in s])

    with open("p042_words.txt") as f:
        contents = f.read().replace("\"", "").split(",")
    
    word_scores = [get_word_score(i) for i in contents]
    
    return [i for i in word_scores if g.contains(i)]