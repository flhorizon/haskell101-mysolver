#!/usr/bin/ruby

argv = ['../mySolver']

tests = [ \
	# Basic / mandatory cases
	'5 * X^0 + 4 * X^1 = 4 * X^0',
	'5 * X^0 + 4 * X^1 = 4 * X^0',
	 '8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0',
	 '42 = 42',
	 '42 - 43 * X^0 = 0',

	# Deg2 single.
	 ' X^2 - 1 + 4X = 2 X',

	# Deg1 neg power
	 ' 5 = X^1',
	# Deg1 neg power
	 ' X^2 = 2 X^-1',
	# Deg2 neg power
	 ' X = 5 X^-1',

	# Deg2 neg power exponential coeff notation
	 ' X - 666.0E-2 X^-1 = -0.0e-1',

	# Deg 4 neg power
	 ' X^-5 - X = 0',

	# Deg 2 Complex
	 'X^2 +  X + 1 = 0',

	# Deg 2 neg power Complex
	 'X + 1 = -X^-1'\
]

sep = 	Proc.new do

		spaces = Array.new( 80, ' '.bytes.first ).pack('C*') 
		nl = [0xA].pack('C')
		dashes = Array.new( 80, '-'.bytes.first ).pack('C*')

		spaces + nl + dashes + nl + spaces
end.call


tests.each do |c|

	cmd = argv.+([' \'', c, '\'']).join
	puts cmd
	system cmd
	puts sep
end
