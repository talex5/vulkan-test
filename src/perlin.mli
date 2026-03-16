(** Noise generator based on Perlin noise (but randomised at startup). *)

val perlin2d : freq:float-> depth:int -> float * float -> float
