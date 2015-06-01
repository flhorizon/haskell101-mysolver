#!/usr/bin/env ruby


argv = ['../mySolver']

tests = [ \
	# Basic / mandatory cases
	'5 * X^0 + 4 * X^1 = 4 * X^0',
	# Out of bounds degree
	 '8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0',
	# |R
	 '42 = 42',
	# None
	 '42 - 43 * X^0 = 0',

	# Deg2 single. 
	 ' X^2 - 1 + 4X = 2 X',

	# Deg1 neg power
	 ' 5 = X^-1',
	# Deg3,  neg power
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


expectations = [ \
	     '-1/4 ; degree 1 speech.',
	     ' Degree too high (3)',
	     '|R',
	     'None: absurd',
	     '-1-sqrt(2) ; sqrt(2)-1 ~> -2.41 ; 0.41',
	     '1/5 ; excluding 0; deg 1',
	     "Shouldn't solve (deg 3)",
	     " sqrt(5), -sqrt(5), excluding 0 ~> +-2.24",
	     "~-> +-2.58 ; excluding 0",
	     ' Degree : will not solve.',
	     '-1/2 +- (Math.sqrt(3) / 2)i',
	     '-1/2 +- (Math.sqrt(3)/ 2)i ; excluding 0'
]


sep = Proc.new do

		spaces = Array.new( 80, ' '.bytes.first ).pack('C*') 
		nl = [0xA].pack('C')
		dashes = Array.new( 80, '-'.bytes.first ).pack('C*')

		spaces + nl + dashes + nl + spaces
end.call


tests.zip( expectations ).each do |p|

	c = p.first
	puts 'Expecting: ' + p.last
	cmd = argv.+([' \'', c, '\'']).join
	puts cmd
	system cmd
	puts sep
end
