
import pandas as pd

hotel_data = pd.read_csv("hotel_bookings.csv")
weather_data = pd.read_csv("weather.csv")
country_data = pd.read_csv("country.csv")

country_data.columns.values[1] = "country2"
country_data.columns.values[0] = "country"


cols = ["country"]

data = hotel_data.join(country_data.set_index(cols), on=cols)



data.to_csv('hotel_data.csv')