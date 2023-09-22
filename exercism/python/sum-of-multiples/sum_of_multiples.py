def sum_of_multiples(limit: int, multiples: list[int]) -> int:
    """Return the sum of all multiples of the given numbers below the limit."""

    return sum(set(num for i in multiples if i > 0 for num in range(i, limit, i)))
