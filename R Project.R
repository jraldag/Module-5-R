# Pull train.csv data into R.
spaceship_titanic <- read_csv("C:\\Users\\jrald\\Documents\\Data Analytics\\Module 5 - R\\R Project\\train.csv")


# View all train.csv data.
view(spaceship_titanic)


# Begin data analysis.


# Total number of passengers.
passenger_count=nrow(spaceship_titanic)
cat("Number of passengers: ", passenger_count)


# Review the total number of passenger groups.
spaceship_titanic$Group <- substring(spaceship_titanic$PassengerId,1,4)
group_count=n_distinct(spaceship_titanic$Group)
cat("Number of groups: ",group_count)


# Graph a breakdown of passenger ages.
spaceship_titanic$AgeGroup <- as.character(cut(spaceship_titanic$Age,
                                               breaks = c(-1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
                                               labels = c("0-5","6-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90","91-95","96-100"),
                                               right = TRUE))
ggplot(spaceship_titanic, aes(
  x = fct_relevel(
    AgeGroup,
    "0-5",
    "6-10",
    "11-15",
    "16-20",
    "21-25",
    "26-30",
    "31-35",
    "36-40",
    "41-45",
    "46-50",
    "51-55",
    "56-60",
    "61-65",
    "66-70",
    "71-75",
    "76-80",
    "81-85",
    "86-90",
    "91-95",
    "96-100"
  ),
  fill = AgeGroup
)) +
  geom_bar() +
  theme(legend.position ="none") +
  xlab("Age Group")


# Get count of VIP passengers.
spaceship_titanic %>%
  group_by(VIP) %>%
  summarize(Count = n())


# Get count of passengers that were transported.
spaceship_titanic %>%
  group_by(Transported) %>%
  summarize(Count = n())


# Get total number of passengers that went into cryosleep.
spaceship_titanic %>%
  group_by(CryoSleep) %>%
  summarize(Count = n())


# Sum amounts billed for luxury amenities by passenger.
spaceship_titanic <- spaceship_titanic %>%
  mutate(RoomService = ifelse(is.na(RoomService), 0, RoomService))
spaceship_titanic <- spaceship_titanic %>%
  mutate(FoodCourt = ifelse(is.na(FoodCourt), 0, FoodCourt))
spaceship_titanic <- spaceship_titanic %>%
  mutate(ShoppingMall = ifelse(is.na(ShoppingMall), 0, ShoppingMall))
spaceship_titanic <- spaceship_titanic %>%
  mutate(Spa = ifelse(is.na(Spa), 0, Spa))
spaceship_titanic <- spaceship_titanic %>%
  mutate(VRDeck = ifelse(is.na(VRDeck), 0, VRDeck))

spaceship_titanic$AmenitiesSum=rowSums(cbind(spaceship_titanic$RoomService,spaceship_titanic$FoodCourt,spaceship_titanic$ShoppingMall,spaceship_titanic$Spa,spaceship_titanic$VRDeck))
df <- data.frame(spaceship_titanic$AmenitiesSum)
colSums(df!=0)


# Graph breakdown of passenger home planets.
spaceship_titanic %>%
  group_by(HomePlanet) %>%
  summarise(Count =  n())

homeplanetcount <- spaceship_titanic %>%
  group_by(HomePlanet) %>%
  summarize(Count = n())
ggplot(homeplanetcount, aes(x="", y=Count, fill=HomePlanet)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()


# Graph breakdown of passenger destinations.
spaceship_titanic %>%
  group_by(Destination) %>%
  summarize(Count = n())

destinationcount <- spaceship_titanic %>%
  group_by(Destination) %>%
  summarize(Count = n())
ggplot(destinationcount, aes(x="", y=Count, fill=Destination)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()


# Graph home planet and destination.
spaceship_titanic$HomePlanet_Destination <- paste(spaceship_titanic$HomePlanet, "to", spaceship_titanic$Destination)
Home_Destination=spaceship_titanic %>%
  drop_na(HomePlanet) %>%
  drop_na(Destination)

spaceship_titanic %>%
  group_by(HomePlanet_Destination) %>%
  summarize(Count = n())

ggplot(data = Home_Destination, mapping = aes(x = HomePlanet_Destination, fill = HomePlanet_Destination)) +
  geom_bar()


# Create graph showing transported by home planet and destination.
spaceship_titanic %>%
  drop_na() %>%
  group_by(HomePlanet_Destination, Transported) %>%
  summarize(Count = n())

ggplot(transported.overview, aes(fill = Transported, y = Count, x = HomePlanet_Destination, label = Count)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(size = 3, position = position_stack())


# Get home planet and destination for those that went into cryosleep.
cryosleep <- spaceship_titanic %>%
  filter(CryoSleep=="TRUE")

cryosleep %>%
  group_by(HomePlanet, Destination) %>%
  summarize(Count = n())


# Get cryosleep count by home planet.
cryosleep %>%
  group_by(HomePlanet) %>%
  summarize(Count = n())

# Get cryosleep count by destination.
cryosleep %>%
  group_by(Destination) %>%
  summarize(Count = n())


# Create graph showing cryosleep by home planet and destination.
spaceship_titanic %>%
  drop_na() %>%
  group_by(HomePlanet_Destination, CryoSleep) %>%
  summarize(Count = n())

ggplot(cryosleep.overview, aes(fill = CryoSleep, y = Count, x = HomePlanet_Destination, label = Count)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(size = 3, position = position_stack())


# Get cryosleep and transported information.
spaceship_titanic %>%
  drop_na() %>%
  group_by(CryoSleep, Transported) %>%
  summarize(Count = n())


# Get total luxury amenities billed; by amenity and in total.
sum(spaceship_titanic$RoomService)
sum(spaceship_titanic$FoodCourt)
sum(spaceship_titanic$ShoppingMall)
sum(spaceship_titanic$Spa)
sum(spaceship_titanic$VRDeck)
sum(spaceship_titanic$RoomService,spaceship_titanic$FoodCourt,spaceship_titanic$ShoppingMall,spaceship_titanic$Spa,spaceship_titanic$VRDeck)

AmenitiesBilled <- data.frame(
  LuxuryAmenity = c("Room Service", "Food Court", "Shopping Mall", "Spa", "VR Deck"),
  AmountBilled = c(1912541, 3898237, 1474092, 2647791, 2592790),
  stringsAsFactors = FALSE
)

ggplot(AmenitiesBilled, aes(x = LuxuryAmenity, y = AmountBilled)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_y_continuous(breaks=c(0,500000,1000000,1500000,2000000,2500000,3000000,3500000,4000000)) +
  xlab("Luxury Amenity") +
  ylab("Amount Billed")

