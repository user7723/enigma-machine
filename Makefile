all:
	stack build && stack install

prof:
	stack build --profile && stack install --profile
