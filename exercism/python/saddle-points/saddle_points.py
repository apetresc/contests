def saddle_points(matrix):
    if len(set(len(row) for row in matrix)) > 1:
        raise ValueError("irregular matrix")

    max_in_row = [
        (x, y)
        for x, row in enumerate(matrix)
        for y, val in enumerate(row)
        if val == max(row)
    ]
    min_in_col = [
        (x, y)
        for x, row in enumerate(matrix)
        for y, val in enumerate(row)
        if val == min(matrix[i][y] for i in range(len(matrix)))
    ]
    return [
        {"row": x + 1, "column": y + 1} for x, y in set(max_in_row) & set(min_in_col)
    ]
