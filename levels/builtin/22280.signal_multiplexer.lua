-- The function get_name() should return a single string that is the name of the puzzle.
--
function get_name()
	return "SIGNAL MULTIPLEXER"
end

-- The function get_description() should return an array of strings, where each string is
-- a line of description for the puzzle. The text you return from get_description() will 
-- be automatically formatted and wrapped to fit inside the puzzle information box.
--
function get_description()
	return {
        "READ VALUES FROM IN.A AND IN.B",
        "READ A VALUE FROM IN.S",
        "WRITE IN.A WHEN IN.S = -1",
        "WRITE IN.B WHEN IN.S = 1",
        "WRITE IN.A + IN.B WHEN IN.S = 0",
    }
end

-- The function get_streams() should return an array of streams. Each stream is described
-- by an array with exactly four values: a STREAM_* value, a name, a position, and an array
-- of integer values between -999 and 999 inclusive.
--
-- STREAM_INPUT: An input stream containing up to 39 numerical values.
-- STREAM_OUTPUT: An output stream containing up to 39 numerical values.
-- STREAM_IMAGE: An image output stream, containing exactly 30*18 numerical values between 0
--               and 4, representing the full set of "pixels" for the target image.
--
-- NOTE: Arrays in Lua are implemented as tables (dictionaries) with integer keys that start
--       at 1 by convention. The sample code below creates an input array of 39 random values
--       and an output array that doubles all of the input values.
--
-- NOTE: To generate random values you should use math.random(). However, you SHOULD NOT seed
--       the random number generator with a new seed value, as that is how TIS-100 ensures that
--       the first test run is consistent for all users, and thus something that allows for the
--       comparison of cycle scores.
--
-- NOTE: Position values for streams should be between 0 and 3, which correspond to the far
--       left and far right of the TIS-100 segment grid. Input streams will be automatically
--       placed on the top, while output and image streams will be placed on the bottom.
--
function get_streams()
    math.randomseed(22280)

    a = {}
    b = {}
    s = {}
    output = {}
    for _ = 0,38 do
        x = math.random(100) - 50
        y = math.random(100) - 50
        z = math.random(3) - 2

        table.insert(a, x)
        table.insert(b, y)
        table.insert(s, z)
        
        if z == -1 then
            table.insert(output, x)
        elseif z == 1 then
            table.insert(output, y)
        else
            table.insert(output, x + y)
        end
    end
	return {
		{ STREAM_INPUT, "IN.A", 1, a },
		{ STREAM_INPUT, "IN.S", 2, s },
		{ STREAM_INPUT, "IN.B", 3, b },
		{ STREAM_OUTPUT, "OUT", 2, output },
	}
end

-- The function get_layout() should return an array of exactly 12 TILE_* values, which
-- describe the layout and type of tiles found in the puzzle.
--
-- TILE_COMPUTE: A basic execution node (node type T21).
-- TILE_MEMORY: A stack memory node (node type T30).
-- TILE_DAMAGED: A damaged execution node, which acts as an obstacle.
--
function get_layout()
	return { 
		TILE_COMPUTE, 	TILE_COMPUTE, 	TILE_COMPUTE, 	TILE_COMPUTE,
		TILE_COMPUTE, 	TILE_COMPUTE,	TILE_COMPUTE, 	TILE_COMPUTE,
		TILE_DAMAGED, 	TILE_COMPUTE,	TILE_COMPUTE, 	TILE_COMPUTE,
	}
end