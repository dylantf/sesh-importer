type sport =
  | Kiteboarding
  | SUP
  | Skiing
  | Snowboarding
  | MountainBiking
  | Hiking
  | Running
  | Paragliding
  | Surfing
  | WingFoiling
  | Parawinging

type sesh_type =
  | Spot
  | Downwinder
  | Roundwinder

type board_type =
  | Twintip
  | Hydrofoil
  | Surfboard
  | SUPBoard
  | Skis
  | Snowboard
  | Other

type normalized = {
  date : string;  (* YYYY-MM-DD *)
  sport : sport;
  hours : float;
  wind_avg : int option;
  wind_gust : int option;
  kite_size : string list option;
  wing_size : string list option;
  sesh_type : sesh_type option;
  board_type : board_type list option;
  foil : string list option;
  board : string option;
  location : string option;
  comments : string option;
}
