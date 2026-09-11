module FoodChain

let fullSong: string list = [
     "I know an old lady who swallowed a cow.";
     "I don't know how she swallowed a cow!";
     "She swallowed the cow to catch the goat.";
     "She swallowed the goat to catch the dog.";
     "She swallowed the dog to catch the cat.";
     "She swallowed the cat to catch the bird.";
     "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.";
     "She swallowed the spider to catch the fly.";
     "I don't know why she swallowed the fly. Perhaps she'll die.";
     "";
     "I know an old lady who swallowed a horse.";
     "She's dead, of course!" ]

let recite start stop: string list =
    [
        for i in start .. stop do
            yield! fullSong.[i * 9 .. (i + 1) * 9 - 1]
    ]