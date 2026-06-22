import pandas as pd
import plotly.graph_objects as go

# Data Alcohol Use Disorders Identification Test (AUDIT)
data = {
    "Lancet Commission Traits": [
        "Education", "Dyslipidemia", "Blood pressure", "Excessive Alcohol Consumption",
        "Diabetes", "Obesity", "Depression", "Physical Inactivity", "Smoking",
        "Hearing Loss", "Social Isolation", "Air Pollution", "Visual Loss", "Traumatic Brain Injury"
    ],
    "GWAS": [
        "Educational Attainment (Okbay2022)",
        "HDL, LDL, Total cholesterol, Triglycerides (Willer2013)",
        "Diastolic, Systolic BP, Pulse Pressure (Evangelou2018)",
        "Drinking Weekly (Lui2019)",
        "Type 2 Diabetes (Xu2018)",
        "BMI (Yengo2019)",
        "Depression (Howard2018)",
        "Physical Inactivity (Klimentidis2018)",
        "Smoking Initiation (Lui2019)",
        "Hearing Difficulties (Wells2019)",
        "Social Isolation (Day2018)",
        "Not Available",
        "Not Available",
        "Not Available"
    ]
}

df = pd.DataFrame(data)

# Calculate dynamic height: 80px per row
row_height = 60

fig_height = (len(df) * row_height) + 100

# Add manual line break in headers to increase height
headers = ["Lancet Commission Trait<br>", "GWAS<br>"]


# Create table
fig = go.Figure(data=[go.Table(
    columnwidth=[5, 10],  # Adjust width if needed
    header=dict(
        values=headers,
        fill_color='lightblue',
        align='left',
        font=dict(size=30, family="Arial"),
        height=15,  # Increase header row height
    ),
    cells=dict(
        values=[df[col] for col in df.columns],
        fill_color='white',
        align='left',
        font=dict(size=28, family="Arial"),
        height=row_height
    )
)])

# Update layout
fig.update_layout(
    autosize=False,
    width=1600,
    height=fig_height
)

# Save as high-res PNG
fig.write_image("lancet_commission_table.png", scale=3, width=1600, height=fig_height+200)

# Show in browser
fig.show()