from espn_api.football import League, league
import pandas as pd

#year = 2009

#pfl = League(league_id=601104, year=year)

#team = pfl.teams[0]
#week = 1
#box_scores = pfl.box_scores(1)

#print(box_scores[0].away_lineup[0].name)
#print(box_scores[0].away_lineup[0].points)

year = 2021

pfl = League(league_id=601104, year=year, espn_s2='AEAhMTKSbynuWm1ZstYqBciLUlw4vw8wHtqrbWme8kY75dedIyO7XXY8xuYXaqc02StiiH%2BtHYdx6R4PW327WFVFGOCaOoC9XOQyIpHS0yLlkpK3KoMfUO5DCiojgL4mMrAb2AoT94wgJXFyCeI53u0ffi8CKf%2BBEdelksk1w7EjLNzPPsiN4T6pj6%2FR0FO0kIQ5PcSSpC02ApvREtrTjQbCnlY0SaJF11CzX%2Fed0BRA6v%2Fyy5neGfg8NwRcYOKLM1g%2BnH9hJF21x5cEv8f6DKw5', swid='{A8A3FC33-0783-4939-BF90-283006B1FBC4}')

box_scores = pfl.box_scores(pfl.current_week)


mdf = pd.read_csv('espn_dat.csv')
gdf = pd.read_csv('espn_mat.csv')

for week in range(1, pfl.current_week+1):
    print(week)
    box_scores = pfl.box_scores(week)
    for matchup in range(0, len(box_scores)):
        home = box_scores[matchup].home_lineup
        away = box_scores[matchup].away_lineup
        for i in range(0,len(home)):
            mdf = mdf.append({'Year': year, 'Week': week, 'Team': box_scores[matchup].home_team.owner,
                'TeamName': box_scores[matchup].home_team.team_name,
                'Player': home[i].name, 'Position': home[i].position, 'Slot': home[i].slot_position,
                'Points': home[i].points, 'Projected': home[i].projected_points}, ignore_index=True)
        for i in range(0,len(away)):
            mdf = mdf.append({'Year': year, 'Week': week, 'Team': box_scores[matchup].away_team.owner,
                'TeamName': box_scores[matchup].away_team.team_name,
                'Player': away[i].name, 'Position': away[i].position, 'Slot': away[i].slot_position,
                'Points': away[i].points, 'Projected': away[i].projected_points}, ignore_index=True)
    matchups = pfl.scoreboard(week)
    for matchup in range(0, len(matchups)):
        if matchups[matchup].home_score > matchups[matchup].away_score:
            h_res = 'W'
            a_res = 'L'
        elif matchups[matchup].home_score < matchups[matchup].away_score:
            h_res = 'L'
            a_res = 'W'
        else:
            h_res = 'D'
            a_res = 'D'
        print(matchups[matchup].home_team)
        try:
            gdf = gdf.append({'Year': year, 'Week': week, 'Team': matchups[matchup].home_team.owner, 
            'TeamName': matchups[matchup].home_team.team_name, 'Opponent': matchups[matchup].away_team.owner,
            'Score': matchups[matchup].home_score, 'Opp_Score': matchups[matchup].away_score, 'Result': h_res}, ignore_index=True)
            gdf = gdf.append({'Year': year, 'Week': week, 'Team': matchups[matchup].away_team.owner, 
            'TeamName': matchups[matchup].away_team.team_name, 'Opponent': matchups[matchup].home_team.owner,
            'Score': matchups[matchup].away_score, 'Opp_Score': matchups[matchup].home_score, 'Result': a_res}, ignore_index=True)
        except AttributeError:
            pass
    

mdf = mdf.replace({'Max Hammersmith': 'HAMM', 'john capannari': 'CAP', 'ryan schwiers': 'RYAN',
    'Ben Bischof': 'BISCH', 'Nick Duke': 'DUKE', 'Ben James': 'JAMES', 'Brad Murphy': 'MURPH', 
    'Mason Loth': 'LOTH', 'Andrew Fieler': 'FIELER', 'Craig Mullen': 'CRAIG', 'Eric Huff': 'ERIC',
    'Collin Schwiers': 'COLLIN', 'Drew Klenk': 'KLENK', 'Cameron Jameson': 'TEPE', 'max kern': 'KERN',
    'Matt Peters': 'PETERS', 'nick botuchis': 'MEADE', 'James Duke': 'DUKE', 'Joshua Rhoads': 'HOFFY',
    'Ben Stauss': 'LOTH', 'cole  tepe': 'TEPE', 'kevin spurlock': 'KEVIN', 'mitch schoener': 'MITCH',
    'jake tiernan': 'TIERNAN', 'Drew Robb': 'COLLIN', 'Holt McDougal': 'KYLE', 'g cappz': 'GREG', 'Jake Bono': 'BONO',
    'ryan  schwiers': 'RYAN', 'Eric  Huff': 'ERIC', 'Collin  Schwiers': 'COLLIN', 'Nate  Campbell': 'SOUP',
    'Nick Meade': 'MEADE', 'Max kern': 'KERN', 'Jack Hammersmith': 'JACKROSS', 'Luke Jett': 'JETT',
    'Andrew  Fieler': 'FIELER', 'john  capannari': 'CAP', 'Brad  Murphy':'MURPH'})
gdf = gdf.replace({'Max Hammersmith': 'HAMM', 'john capannari': 'CAP', 'ryan schwiers': 'RYAN',
    'Ben Bischof': 'BISCH', 'Nick Duke': 'DUKE', 'Ben James': 'JAMES', 'Brad Murphy': 'MURPH', 
    'Mason Loth': 'LOTH', 'Andrew Fieler': 'FIELER', 'Craig Mullen': 'CRAIG', 'Eric Huff': 'ERIC',
    'Collin Schwiers': 'COLLIN', 'Drew Klenk': 'KLENK', 'Cameron Jameson': 'TEPE', 'max kern': 'KERN',
    'Matt Peters': 'PETERS', 'nick botuchis': 'MEADE', 'James Duke': 'DUKE', 'Joshua Rhoads': 'HOFFY',
    'Ben Stauss': 'LOTH', 'cole  tepe': 'TEPE', 'kevin spurlock': 'KEVIN', 'mitch schoener': 'MITCH',
    'jake tiernan': 'TIERNAN', 'Drew Robb': 'COLLIN', 'Holt McDougal': 'KYLE', 'g cappz': 'GREG', 'Jake Bono': 'BONO',
    'ryan  schwiers': 'RYAN', 'Eric  Huff': 'ERIC', 'Collin  Schwiers': 'COLLIN', 'Nate  Campbell': 'SOUP',
    'Nick Meade': 'MEADE', 'Max kern': 'KERN', 'Jack Hammersmith': 'JACKROSS', 'Luke Jett': 'JETT',
    'Andrew  Fieler': 'FIELER', 'john  capannari': 'CAP', 'Brad  Murphy':'MURPH'})




#print(mdf.head())
mdf.to_csv('espn_dat_live.csv', index=False)
#print(gdf.head())
gdf.to_csv('espn_mat_live.csv')