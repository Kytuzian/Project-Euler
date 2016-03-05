def separate_line(line, f=lambda x: x, combine=None, separator=','):
    if combine:
        return combine(*map(f, line.split(separator)))
    else:
        return map(f, line.split(separator))
    
def log_exponent(base, xp):
    return xp * ln(base)
    
def p99():
    contents = ''
    
    with open('p099_base_exp.txt') as f:
        contents = f.readlines()
        
    log_exp_lines = map(lambda line: separate_line(line, Integer, log_exponent), contents)
    
    return log_exp_lines.index(max(log_exp_lines)) + 1 #Because line numbers start at 1 not 0