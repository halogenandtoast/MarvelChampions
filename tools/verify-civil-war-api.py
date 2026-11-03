"""Exercise a running Civil War API against a disposable test database.

Creates disposable guests and games. Pass the API root, including /api, explicitly.
No credentials, existing games, or production configuration are required.
"""
import argparse
import urllib.request, urllib.error, json

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--api', required=True, help='Isolated API root, e.g. http://localhost:3037/api')
base = parser.parse_args().api.rstrip('/')
token=None
def req(path,data=None,expected=200):
 raw=None if data is None else json.dumps(data).encode()
 headers={'Content-Type':'application/json'}
 if token: headers['Authorization']='Token '+token
 try:
  with urllib.request.urlopen(urllib.request.Request(base+path,data=raw,headers=headers),timeout=40) as r: status,body=r.status,r.read()
 except urllib.error.HTTPError as e: status,body=e.code,e.read()
 assert status==expected,(path,status,body[:350])
 return json.loads(body) if body and status == 200 else None
session=req('/guest',{})
token=session['token']
options=req('/scenarios')['civilWar']
reg=options['registration']['presets'][0];res=options['resistance']['presets'][0]
setup={'scenarioCode':reg['leader'],'modulars':reg['modulars'],'mainSchemes':reg['mainSchemes'],'difficulty':'Normal','competition':{'registration':reg,'resistance':res}}
def create(seats): return req('/games',{'name':'Civil War isolated verification','scenario':setup,'decks':[],'seats':seats})
req('/games',{'name':'invalid','scenario':setup,'decks':[],'seats':3},400)
req('/games',{'name':'invalid-root','scenario':dict(setup, scenarioCode='01094'),'decks':[],'seats':2},400)
print('Invalid three-seat competitive creation rejected')
gid=create(2)['id']
req('/games/'+gid+'/start',{},409)
req('/games/'+gid+'/join',{'deck':{'starter':'iron-man'},'position':0},400)
req('/games/'+gid+'/join',{'deck':{'starter':'captain-america'},'position':1},400)
print('Own-team leader hero rejected')
req('/games/'+gid+'/join',{'deck':{'starter':'spider-man'},'position':1})
req('/games/'+gid+'/start',{},409)
print('Opposing-team seat accepted; partial start rejected')
req('/games/'+gid+'/join',{'deck':{'starter':'thor'},'position':0})
req('/games/'+gid+'/start',{})
print('One account filled both teams and started native competitive game')
print('Verified game:',gid)
view=req('/games/'+gid)
assert view['started'] is True
assert len(view['game']['competition']['teams'])==2
assert len(view['game']['areas'])==2
assert [s['position'] for s in view['table']['seated']]==[0,1]
assert all(s['userId']==session['user']['id'] for s in view['table']['seated'])
print('Live public view has two team boards, preserved seat order and shared account ownership')
gid4=create(4)['id']
for position,hero in [(3,'hawkeye'),(0,'spider-man'),(2,'black-panther'),(1,'thor')]:
 req('/games/'+gid4+'/join',{'deck':{'starter':hero},'position':position})
 pending=req('/games/'+gid4)
 assert not pending['started']
 assert pending['table']['competition']==setup['competition']
req('/games/'+gid4+'/start',{})
view4=req('/games/'+gid4)
assert view4['started']
assert [s['position'] for s in view4['table']['seated']]==[0,1,2,3]
assert sorted(len(t['roster']) for t in view4['game']['competition']['teams'])==[2,2]
print('Four seats joined out of order; full lobby waited for host; reconnect preserves balanced team rosters')
# Separate accounts must see only their own team's private choices and hand.
owner_token = token
other_session = req('/guest', {})
other_token = other_session['token']
gid_private = create(2)['id']
req('/games/'+gid_private+'/join', {'deck': {'starter': 'spider-man'}, 'position': 0})
token = other_token
req('/games/'+gid_private+'/join', {'deck': {'starter': 'thor'}, 'position': 1})
token = owner_token
req('/games/'+gid_private+'/start', {})
for step in range(80):
    answered = False
    for viewer_token in [owner_token, other_token]:
        token = viewer_token
        state = req('/games/'+gid_private)
        game = state['game']
        playable = any(choice.get('kind') == 'endTurn' for question in game['question'].values() for choice in question['choices'])
        if playable:
            break
        for player, question in game['question'].items():
            if question['choices']:
                choice = question['choices'][0]
                req('/games/'+gid_private, {'player': player, 'questionVersion': game['questionVersion'], 'choice': choice['index']})
                answered = True
                break
        if answered:
            break
    if playable:
        break
else:
    raise AssertionError('Private-account setup did not reach a player turn')
for viewer_token in [owner_token, other_token]:
    token = viewer_token
    state = req('/games/'+gid_private)
    visible_hands = [p for p in state['game']['players'] if p.get('hand')]
    assert len(visible_hands) == 1
    visible_id = visible_hands[0]['id']
    assert all(not q['choices'] for player, q in state['game']['question'].items() if player != visible_id)
print('Separate-account views expose only the owned hand and conceal opposing choices')
token = owner_token
