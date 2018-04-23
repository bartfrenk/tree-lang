

if $creative.size == "640x480":
    if $geo.city == "Amsterdam":
        pancake = "amsterdam-pancake.png"
        title = "Amsterdam!"
    elif $geo.city == "Paris":
        pancake = "paris-pancake.png"
    else:
        pancake = "small-pancake.png"
    end
end

if $time.part == "evening":
    title = "Good evening"
else:
    title = "Good day!"
end;