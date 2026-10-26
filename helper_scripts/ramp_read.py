#What to run to get current operational settings
#python3 ramp_read.py '10_23_26' -m '[[1,1],[1,1],[1,1]]' -m '[[1,1],[1,1],[1,1]]' -v '[2,7]' -v '[1]'

import pacsys
from pacsys.ramp import BoosterHVRamp, BoosterQRamp, BoosterSQRamp

import matplotlib.pyplot as plt
import numpy as np
import json

import argparse

import device_data as dd

print("THIS MODULE READS FROM CONTROL SYSTEM ONLY")
print("THIS MODULE DOES NOT WRITE TO THE CONTROL SYSTEM")

###################
deviceList = []
for n in range(0,24):
    numstr=repr(n+1)
    deviceList.append("B:HS"+numstr)#+"T")
    deviceList.append("B:HL"+numstr)#+"T")
diplHorzDeviceList=deviceList

deviceList = []
for n in range(0,24):
    numstr=repr(n+1)
    deviceList.append("B:VS"+numstr)#+"T")
    deviceList.append("B:VL"+numstr)#+"T")
diplVertDeviceList=deviceList

deviceList = []
for n in range(0,24):
    numstr=repr(n+1)
    deviceList.append("B:QL"+numstr)#+"T")
    deviceList.append("B:QS"+numstr)#+"T")
quadNormDeviceList=deviceList

deviceList = []
for n in range(0,24):
    numstr=repr(n+1)
    deviceList.append("B:SQL"+numstr)#+"T")
    deviceList.append("B:SQS"+numstr)#+"T")
quadSkewDeviceList=deviceList

deviceList = []
for n in range(0,24):
    numstr=repr(n+1)
    deviceList.append("B:SXS"+numstr)#+"T")
    deviceList.append("B:SXL"+numstr)#+"T")
sextNormDeviceList=deviceList

deviceList = []
for n in range(0,24):
    numstr=repr(n+1)
    deviceList.append("B:SSL"+numstr)#+"T")
    deviceList.append("B:SSS"+numstr)#+"T")
sextSkewDeviceList=deviceList

#Settings
###################
bmaDeviceNames = [[diplHorzDeviceList,diplVertDeviceList],[quadNormDeviceList,quadSkewDeviceList],[sextNormDeviceList,sextSkewDeviceList]]

p=argparse.ArgumentParser()
p.add_argument("name")
p.add_argument("-m", "--matrix", type=json.loads, action="append")
p.add_argument("-v", "--vec", type=json.loads, action="append")
args=p.parse_args()

print(args)

frontmatter=args.name
bma_T_table=args.matrix[0]
bma_O_table=args.matrix[1]
bflat_table=args.vec[0]
bex_table=args.vec[1]
###################

#BMA Ramps
#####################################################################################################################################
ramp_dict = {}
#Will both include array ramps 'T' and offset ramps 'O'
#Make them separate devices for this to work

#scale factor setting[n-1] corresponds to value 0n
#ramp array slot=n-1 corresponds to table 0n table
#offset value setting[n-1] corresponds to value 0n

sf = pacsys.read("B:BFLAT"+"S"+'.SETTING['+str(bflat_table[0]-1)+']')
ramps = pacsys.ramp.BoosterHVRamp.read("B:BFLAT"+"T", slot=bflat_table[1]-1)
ramp_dict["B:BFLAT"+"S"] = {'sf':sf, 'kind':'BFLAT', 'slot':bflat_table[0], 'role':'scale', 'group':'B:BFLAT'}
ramp_dict["B:BFLAT"+"T"] = {'values':ramps.values, 'dt':ramps.times, 't':ramps.cumtimes, 'dtoff':156.0, 'slot':bflat_table[1], 'role':'table', 'kind':'BFLAT', 'group':'B:BFLAT', 'unit':'A'}

for i in range(0,3):
    for j in range(0,2):
        
        # List comprehension
        rmp_dev_T = [rmp_dev + "T" for rmp_dev in bmaDeviceNames[i][j]]
        rmp_dev_O = [rmp_dev + "O" for rmp_dev in bmaDeviceNames[i][j]]

        ramps=None

        if(i==1):
            if(j==0):
                ramps = pacsys.ramp.BoosterQRamp.read_many(rmp_dev_T, slot=bma_T_table[i][j]-1)
            else:
                ramps = pacsys.ramp.BoosterSQRamp.read_many(rmp_dev_T, slot=bma_T_table[i][j]-1)
        else:
            ramps = pacsys.ramp.BoosterHVRamp.read_many(rmp_dev_T, slot=bma_T_table[i][j]-1)

        for index, ramp in enumerate(ramps):
            #print(ramp.times)
            
            kind=''
            if(i==0):
                if(j==0):
                    kind='H'
                else:
                    kind='V'
            elif(i==1):
                if(j==0):
                    kind='Q'
                else:
                    kind='SQ'
            else:
                if(j==0):
                    kind='SX'
                else:
                    kind='SS'
                
            ramp_dict[rmp_dev_T[index]] = {'values':ramp.values, 'dt':ramp.times, 't':ramp.cumtimes, 'dtoff':156.0, 'slot':bma_T_table[i][j], 'role':'table',  'kind':kind, 'group':bmaDeviceNames[i][j][index], 'unit':'A'}
            
            offset = pacsys.read(rmp_dev_O[index]+'.SETTING['+str(bma_O_table[i][j]-1)+']')
            ramp_dict[rmp_dev_O[index]] = {'value':offset, 'slot':bma_O_table[i][j], 'role':'offset', 'kind':kind, 'group':bmaDeviceNames[i][j][index], 'unit':'A'}


dd.save(ramp_dict,"../data_Booster/"+frontmatter+"_BMA.json")
            
#Non BMA Devices
#####################################################################################################################################
otherdeviceList = []

other_dict={}

ramp =pacsys.ramp.BoosterHVRamp.read("B:BEX2"+"T", slot=bex_table[0]-1)

other_dict['B:VIMIN'] = {'value':pacsys.read('B:VIMIN.SET'), 'role':'setting', 'kind':'GMPS', 'group':'B:VIMIN', 'unit':'A'}
other_dict['B:VIMAX'] = {'value':pacsys.read('B:VIMAX.SET'), 'role':'setting', 'kind':'GMPS', 'group':'B:VIMAX', 'unit':'A'}

other_dict['B:BEX2T'] = {'values':ramp.values, 'dt':ramp.times, 't':ramp.cumtimes, 'dtoff':156.0, 'slot':bex_table[0], 'role':'table', 'kind':'BEX', 'group':'B:BEX2', 'unit':'A', 'note':'dtoff 156 us as the BMA tables (user 2026-10-25): read with the same ramp card class (BoosterHVRamp) as the H/V correctors, and possibly the same power supplies; the fitted RF extra delay (155.2 us) is similar, so these devices may be synchronized internally. Not measured for BEX2 itself.'}

other_dict['MKSON'] = {'value':0.0, 'role':'setting', 'kind':'MKS', 'group':'MKSON'}#on/off factor, no unit

#BDOT, TFLAT (MEV400) from B:BFLAT, KEMAX from the RF curve fit: timing_fit.py, 2026-10-25 (re-run timing_fit.py --apply after a new read)
other_dict['BDOT'] = {'value':2271.0, 'role':'setting', 'kind':'GMPS', 'group':'BDOT', 'unit':'us'}
other_dict['KEMAX'] = {'value':7.988, 'role':'setting', 'kind':'GMPS', 'group':'KEMAX', 'unit':'GeV'}
other_dict['TFLAT'] = {'value':285.0, 'role':'setting', 'kind':'GMPS', 'group':'TFLAT', 'unit':'us'}
other_dict['CIRC'] = {'value':474.200000001, 'role':'setting', 'kind':'LATTICE', 'group':'CIRC', 'unit':'m'}
other_dict['GMPSFRQ'] = {'value':15.0, 'role':'setting', 'kind':'GMPS', 'group':'GMPSFRQ', 'unit':'Hz'}
dd.save(other_dict,"../data_Booster/"+frontmatter+"_MISC_MEV400.json")
other_dict['TFLAT']['value'] = 400.0

other_dict['CIRC']['value'] = 474.2002212361
other_dict['GMPSFRQ']['value'] = 20.0
other_dict['B:VIMIN']['value'] = 1.5341673048225735*other_dict['B:VIMIN']['value']

dd.save(other_dict,"../data_Booster/"+frontmatter+"_MISC_PIP_II.json")

#RF readbacks Devices
#####################################################################################################################################
rfdeviceList = []

for n in range(0,22):
    numstr=str(n+1)
    if(len(numstr)==1):
        numstr='0'+numstr
    rfdeviceList.append("B:RF"+numstr+"GA[0:49]")

rf_dict={}

ramps=pacsys.get_many(rfdeviceList)

for index, ramp in enumerate(ramps):
    values = ramp.value
    if values is not None:#RF21, RF22 have no data
        values = np.array(values, dtype=float)
        values[41] = 0.0#junk sample at t = 41 ms on every station (seen in every read so far)
        if(index==0):
            values = values/20.0#B:RF01GA reads ~20x the other stations (factor inferred)
    n = 50 if values is None else len(values)#[0:49] is 50 points, one per ms
    rf_dict[rfdeviceList[index][:-6]] = {'values':values, 'dt':np.concatenate([[0.0], np.full(n-1, 1000.0)]), 't':np.arange(n)*1000.0, 'dtoff':0.0, 'role':'table', 'kind':'VRF', 'group':rfdeviceList[index][:-6], 'unit':'kV/T'}#kV per turn

dd.save(rf_dict,"../data_Booster/"+frontmatter+"_VRF_READBACK.json")