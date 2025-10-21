#!/usr/bin/env python3
"""
Fetch hourly weather for Charlotte, NC and save to CSV.

Usage:
  python3 weather.py --start 2025-10-01 --end 2025-10-07 --out charlotte_weather.csv (change the end dates as needed.)

Notes:
- Dates are inclusive; timezone America/New_York.
"""
import argparse
import datetime as dt
import sys
import requests
import pandas as pd

CHARLOTTE_LAT = 35.2271
CHARLOTTE_LON = -80.8431
TIMEZONE = "America/New_York"

HOURLY_VARS = [
    "temperature_2m",
    "relative_humidity_2m",
    "apparent_temperature",
    "precipitation",
    "rain",
    "snowfall",
    "cloud_cover",
    "pressure_msl",
    "wind_speed_10m",
    "wind_gusts_10m",
    "wind_direction_10m",
]

def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--start", required=False, help="Start date YYYY-MM-DD (default: today)")
    p.add_argument("--end", required=False, help="End date YYYY-MM-DD (default: start)")
    p.add_argument("--out", default="charlotte_weather.csv", help="Output CSV path")
    return p.parse_args()

def validate_dates(s, e):
    try:
        sd = dt.date.fromisoformat(s)
        ed = dt.date.fromisoformat(e)
    except ValueError:
        sys.exit("Dates must be YYYY-MM-DD.")
    if ed < sd:
        sys.exit("End date must be on/after start date.")
    return sd, ed

def fetch_hourly(start_date: dt.date, end_date: dt.date) -> pd.DataFrame:
    #free api that didnt need a key
    url = "https://archive-api.open-meteo.com/v1/archive"
    params = {
        "latitude": CHARLOTTE_LAT,
        "longitude": CHARLOTTE_LON,
        "start_date": start_date.isoformat(),
        "end_date": end_date.isoformat(),
        "hourly": ",".join(HOURLY_VARS),
        "timezone": TIMEZONE,
    }
    r = requests.get(url, params=params, timeout=60)
    r.raise_for_status()
    data = r.json()

    if "hourly" not in data or "time" not in data["hourly"]:
        raise RuntimeError("Unexpected response, missing 'hourly' data")

    hourly = data["hourly"]
    df = pd.DataFrame(hourly)
    # bad sort but it works
    df["time"] = pd.to_datetime(df["time"])
    df = df.sort_values("time").reset_index(drop=True)
    return df

def main():
    args = parse_args()
    today = dt.date.today()
    start_str = args.start or today.isoformat()
    end_str = args.end or start_str
    start_date, end_date = validate_dates(start_str, end_str)

    try:
        df = fetch_hourly(start_date, end_date)
    except requests.HTTPError as e:
        sys.exit(f"HTTP error from API: {e}")
    except Exception as e:
        sys.exit(f"Failed to fetch/parse data: {e}")

    df.to_csv(args.out, index=False)
    print(f"Wrote {len(df)} rows to {args.out}")

if __name__ == "__main__":
    main()
