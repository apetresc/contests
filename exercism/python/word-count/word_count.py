import collections
import re

def count_words(sentence: str) -> dict[str,int]:
    """
    Count the number of times each word occurs in a sentence, taking into
    account apostrophes and ignoring punctuation.
    """

    return collections.Counter(re.findall(r"[a-zA-Z0-9]+(?:'[a-zA-Z0-9]+)?", sentence.lower()))

