1 ' ===================================
2 '
3 '  2100301001 STEP5 Assy1プログラム
4 '
5 ' 作成者：自動化T
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
8 ' ===================================
9 '===== <Insight定数> =====
10 '===== <Insight変数定義> =====
11 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
12 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
13 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
14 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
15 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
16 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
17 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
18 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
19 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
20 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
21 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
22 '
23 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
24 'Output Signal
25 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
26 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
27 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
28 '
29 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
30 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
31 '
32 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
33 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
34 '作業用変数
35 Def Inte MInspErrNum                '検査実行エラー番号
36 Def Inte MInspNGStepNum             '検査実行NGStep番号
37 Def Inte MRtn                       'Function戻り値取得用
38 Def Inte MRtn2                      'Function戻り値取得用
39 Def Inte MRet3                      'Function戻り値取得用
40 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
41 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
42 Def Inte MovrdA                     'ネジ締めOvrd 可変用
43 Def Float MSpdA                     'ネジ締めSpd　可変用
44 Def Pos PTemp                       'ネジ締め上空位置計算用
45 '===== <Insight変数設定> =====
46 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
47 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
48 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
49 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
50 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
51 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
52 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
53 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
54 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
55 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
56 'Output Signal
57 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
58 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
59 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
60 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
61 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
62 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
63 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
64 '===== <電ドラ変数定義> =====
65 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
66 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
67 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
68 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
69 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
70 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
71 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
72 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
73 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
74 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
75 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
76 '11584   'toRBトルクドライバ-COMP_ERR送信
77 Y60_Driver=12240 '電ドラ半時計回り CCW
78 Y61_Driver=12241 '電ドラ時計回り CW
79 Y62_Driver=12242 'バンクセッティング BANK C1
80 Y63_Driver=12243 'バンクセッティング BANK C2
81 Y64_Driver=12244 'バンクセッティング BANK C3
82 Y65_Driver=12245 'プログラムセッティング PRG SET F1
83 Y66_Driver=12246 'プログラムセッティング PRG SET F2
84 Y67_Driver=12247 'プログラムセッティング PRG SET F3
85 X34_ScrewReady1=11259 'ねじっこ1　Read
86 '===== <電ドラ定数> =====
87 Dim PScrewPos(10)       'ネジ締め用Function引数変数
88 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
89 Dim PEscapePosi(10)
90 MLoopCnt% = 0'
91 '===== <ロボット定数> =====
92 '===== <ロボット変数定義> =====
93 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
94 MCommentD1001 = 0
95 MCommentD1002 = 0
96 MCommentD1003 = 0
97 MScreenNo = 0
98 '
99 MCommentTSU = 0
100 MCommentTSD = 0
101 'ウィンド画面番号設定
102 MWindReSet = 0
103 MWindInfoScr = 5
104 MWindErrScr = 10
105 MWindErrScr2 = 11
106 MWindErrScr3 = 13
107 MWindErrScr17 = 17
108 MWindErrScr18 = 18
109 MWindCmmnScr = 20
110 MWindJigRelase19049 = 60
111 MWindJigRelase19050 = 61
112 MWindJigRelase19051 = 62
113 '
114 MClear% = 0        'KEY_のクリア
115 MAbout% = 1        'KEY_停止
116 MNext% = 2         'KEY_次のステップへ移行
117 MContinue% = 3     'KEY_継続 再度同じ動作を行う
118 '
119 Def Inte MNgProcess
120 MNgProcess% = 5      'KEY_NG
121 '
122 MAssyOK% = 6       '組立完了
123 MPass% = 7         '工程パス
124 MPiasNG% = 8       'Pias確認時履歴NG
125 '
126 '初期化用KEY番号   '
127 MRobotInit1% = 11  '初期位置用
128 MRobotInit2% = 12  '初期位置用
129 MRobotInit3% = 13  '初期位置用
130 MRobotInit4% = 14  '初期位置用
131 '
132 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
133 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
134 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
135 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
136 '
137 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
138 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
139 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
140 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
141 '
142 MOK% = 1               '各判定用
143 MNG% = 0               '各判定用
144 MTIMEOUT% = -1         '各判定用
145 MJudge% = 0            '判定情報格納用
146 '
147 MRECIVETIME& = 0
148 MSETTIMEOUT10& = 10000&                '10秒設定
149 MSETTIMEOUT03& = 3000&                 '3秒設定
150 MSETTIMEOUT01& = 1000&                 '1秒設定
151 MSETTIMEOUT05& = 5000&                 '5秒設定
152 MSETTIMEOUT009& = 900&                 '0.9秒設定
153 MSETTIMEOUT008& = 800&                 '0.8秒設定
154 MSETTIMEOUT007& = 700&                 '0.7秒設定
155 MSETTIMEOUT006& = 600&                 '0.6秒設定
156 MSETTIMEOUT005& = 500&                 '0.5秒設定
157 MSETTIMEOUT004& = 400&                 '0.4秒設定
158 MSETTIMEOUT003& = 300&                 '0.3秒設定
159 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
160 MIN_PIAS_ComOK% = 11552                'PC通信OK
161 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
162 MIN_PIAS_ComNG% = 11553                'PC通信NG
163 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
164 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
165 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
166 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
167 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
168 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
169 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
170 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
171 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
172 '
173 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
174 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
175 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
176 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
177 '
178 MOUT_PiasAssyResultOK% = 12549    '組立OK
179 MOUT_PiasAssyResultNG% = 12550    '組立NG
180 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
181 '
182 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
183 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
184 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
185 '
186 MIN_Insight_Use% = 11369               '画像確認ON
187 MIN_TorqueCheck% = 11348               'トルクチェック
188 '
189 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
190 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
191 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
192 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
193 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
194 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
195 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
196 '
197 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
198 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
199 '
200 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
201 '
202 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
203 '
204 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
205 MopeNo% = 0
206 MRtn% = 0
207 MRet = 0
208 MRet3% = 0
209 '
210 Def Inte MInputQty          '投入数 演算変数
211 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
212 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
213 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
214 Def Inte nAssyOkQty         '未使用
215 Def Inte MScrewNo
216 Def Inte MReTry
217 '===== <IO変数定義> =====
218 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
219 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
220 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
221 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
222 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
223 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
224 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
225 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
226 '
227 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
228 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
229 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
230 '
231 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
232 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
233 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
234 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
235 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
236 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
237 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
238 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
239 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
240 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
241 '
242 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
243 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
244 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
245 '
246 Def Inte MOUT_LED1          ' 画像処理用LED照明
247 '
248 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
249 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
250 '
251 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
252 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
253 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
254 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
255 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
256 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
257 '
258 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
259 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
260 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
261 '
262 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
263 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
264 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
265 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
266 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
267 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
268 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
269 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ '数値12250から12248へ変更(8/5中村)
270 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
271 MOUT_VB1%   =  12250    ' アーム先端　ネジ吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
272 '
273 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
274 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
275 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
276 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
277 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
278 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
279 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
280 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
281 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
282 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
283 '
284 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
285 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
286 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
287 '
288 MOUT_LED1%  =  12239    ' 画像処理用LED照明
289 '
290 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
291 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
292 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
293 '
294 '共通
295 Def Inte MTEST_KEY                      'デバックテスト用
296 Def Inte MOn                            '出力=1
297 Def Inte MOff                           '出力=0
298 '
299 'ねじ締め装置_出力アドレス
300 Def Inte MOUT_ScwT_ComChk               '通信確認
301 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
302 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
303 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
304 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
305 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
306 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
307 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
308 'ねじ締め装置_入力アドレス
309 Def Inte MIN_ScwT_comOK                 '通信確認返信
310 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
311 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
312 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
313 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
314 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
315 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
316 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
317 '
318 Dim MScwT_Case1%(2)               '条件1停止変数
319 Dim MScwT_Case2%(2)               '条件2停止変数
320 Dim MScwT_Case3%(2)               '条件3停止変数
321 Dim MScwT_Case4%(2)               '条件4停止変数
322 Dim MScwT_Case5%(2)               '条件5停止変数
323 '
324 '共通
325 MTEST_KEY% = 11359                       'デバッグ用テストKEY
326 MOn% = 1                                 '出力 = 1
327 MOff% = 0                                '出力 = 0
328 '
329 'ねじ締め機_アドレス設定
330 MOUT_ScwT_ComChk% = 12816               '通信確認送信
331 MOUT_ScwT_ST% = 12849                   'ねじ締め開始を送信
332 MOUT_ScwT_ReSTOK% = 12850               '再開始受信を送信
333 MOUT_ScwT_FinOK% = 12852                'ねじ締め完了受信を送信
334 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
335 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
336 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
337 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
338 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
339 '
340 MIN_ScwT_comOK% = 11824                 'ねじ締め装置から返信
341 MIN_ScwT_STRec% = 11857                 'ねじ締め開始を受信
342 MIN_ScwT_ReST% = 11858                  '再開始を受信
343 MIN_ScwT_Fin% = 11860                   'ねじ締め完了を受信
344 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
345 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
346 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
347 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
348 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
349 '
350 MScwT_Case1%(1) = MIN_ScwT_Case1%
351 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
352 MScwT_Case2%(1) = MIN_ScwT_Case2%
353 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
354 MScwT_Case3%(1) = MIN_ScwT_Case3%
355 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
356 MScwT_Case4%(1) = MIN_ScwT_Case4%
357 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
358 MScwT_Case5%(1) = MIN_ScwT_Case5%
359 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
360 '
361 '設定 InitialZoneBで使用する変数
362 Def Pos PActive       '直交座標系 位置変数 現在位置
363 Def Pos Pmove         '直交座標系 位置変数 移動先
364 Def Jnt JActive       '関節座標系 位置変数 現在位置
365 Def Jnt Jmove         '関節座標系 位置変数 移動先
366 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
367 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
368 '★注意★初期位置を変更した時には、変更が必要！
369 '
370 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
371 Function M% fnAssyStart
372     M_25# = 0
373     M_26# = 0
374 ' PIASチケット読込み工程抜け確認
375     M_20# = MClear%                       '初期化
376 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
377 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
378 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
379 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
380 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
381 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
382 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
383 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
384 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
385 '    EndIf
386     ' ネジ締め機テスト用 ----------
387     '    'Mret% = fScewTcomChk()
388     '    'ねじ締め開始
389     '    fScewTStart()
390     '    '
391     '    '座標移動
392     '    '
393     '    '条件xx停止
394     '    fScewTCaseStop(MScwT_Case5%)
395     '    '
396     '    'ベースユニットKEY
397     '    Wait M_In(MTEST_KEY%) = MOn%
398     '    '
399     '    '再開始
400     '    fScewTReStart()
401     '    '
402     '    '座標移動
403     '    '
404     '    'ねじ締め完了
405     '    Mret% = fScewTFinish()
406     ' ネジ締めテスト終了
407     ' PIASテスト -----------
408     '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
409     '    MRet% = fnPiasWrite(MNG%)
410     '    MRet% = fnPCBNumberCheck()
411     ' PIASテスト終了 -------
412     '組み立て開始
413     'プログラム原点
414         '初期位置を設定
415     PTemp = P_Curr
416     MRtn = 0
417 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
418 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
419 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
420 '                MRtn = 1
421 '            EndIf
422 '        EndIf
423 '    EndIf
424 '    If MRtn = 1 Then
425 '        M_Out(12269) = 0            '位置決め戻OFF
426 '        M_Out(12268) = 1            '位置決め出ON
427 '        Mov PTicketRead
428 '    Else
429 '        Mov PInitialPosition
430 '        M_Out(12269) = 0            '位置決め戻OFF
431 '        M_Out(12268) = 1            '位置決め出ON
432 '        Mov PTicketRead_1           'チケットID読み取り回避点
433 '        Mvs PTicketRead             'ID読み位置
434 '    EndIf
435 '
436 ' 2022/04/04 安全方向へ条件変更 渡辺
437 ' PInitialPosition 在席 MRtn=2
438 ' PTicketRead_1 在席 MRtn=1
439 '
440     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
441         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
442             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
443                 MRtn = 2
444             EndIf
445         EndIf
446     EndIf
447     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
448         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
449             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
450                 MRtn = 1
451             EndIf
452         EndIf
453     EndIf
454     fnAutoScreenComment(521)        '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
455     If MRtn = 2 Then
456         M_Out(12269) = 0            '位置決め戻OFF
457         M_Out(12268) = 1            '位置決め出ON
458         Mov PTicketRead_1           'チケットID読み取り回避点
459         Mvs PTicketRead             'ID読み位置
460     Else
461         If MRtn = 1 Then
462             M_Out(12269) = 0            '位置決め戻OFF
463             M_Out(12268) = 1            '位置決め出ON
464             Mvs PTicketRead             'ID読み位置
465         Else
466             fErrorProcess(11,230,281,0)    'エラー停止
467         If M_20# = MNext% Then GoTo *ASSY_ERROR_END
468             If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
469             If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
470             If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
471         EndIf
472     EndIf
473 '
474     Ovrd 100
475     'ハンド及び治具に本体が無いか
476     *INITIAL_CHECK
477     If M_In(11264) =0 And M_In(11269) = 0 Then GoTo *CompInitial
478     fErrorProcess(11,253,281,0)
479     If M_20# = MNext% Then M_20# = MClear%
480     If M_20# = MNgProcess% Then M_20# = MAbout%
481     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
482     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
483     *CompInitial
484 '
485     '治具を初期位置に戻す(追加11/19中村)
486     *RE_JIG_INI
487     MRtn = 1
488     MRtn2 = 1
489     If M_In(11276) = 0 Or M_In(11277) = 0 Then  '回転軸がセンターに来ていなければ
490         M_Out(12258) = 0        '製品チャック開OFF
491         M_Out(12259) = 1        '製品チャック閉ON
492         MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    '製品チャック閉検出
493         M_Out(12262) = 0        '回転ストッパー出OFF
494         M_Out(12263) = 1        '回転ストッパー戻ON
495         MRtn2 = frInCheck(11275,1,MSETTIMEOUT05&)    '回転ストッパー戻端検出
496     EndIf
497     If MRtn = 1 And MRtn2 = 1 Then GoTo *CompJigIni1
498     fErrorProcess(11,262,284,0) '0→262に変更6/3中村
499     If M_20# = MNext% Then M_20# = MClear%
500     If M_20# = MNgProcess% Then M_20# = MAbout%
501     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
502     If M_20# = MContinue% Then GoTo *RE_JIG_INI
503 *CompJigIni1
504     '
505     If M_In(11278) = 1 Then     'CW端に回転していたなら
506         M_Out(12266) = 0        'バルブ2ONをOFF
507         M_Out(12267) = 1 Dly 0.3       'バルブ2OFFをON
508         Dly 0.3
509         M_Out(12264) = 0        'バルブ1ONをOFF
510         M_Out(12265) = 1 Dly 0.3       'バルブ1OFFをON
511     ElseIf M_In(11279) = 1 Then 'CCW端に回転していたなら
512         M_Out(12264) = 0        'バルブ1ONをOFF
513         M_Out(12265) = 1 Dly 0.3       'バルブ1OFFをON
514         Dly 0.3
515         M_Out(12266) = 0        'バルブ2ONをOFF
516         M_Out(12267) = 1 Dly 0.3       'バルブ2OFFをON
517     Else
518         M_Out(12264) = 0        'バルブ1ONをOFF
519         M_Out(12265) = 1 Dly 0.3       'バルブ1OFFをON
520         M_Out(12266) = 0        'バルブ2ONをOFF
521         M_Out(12267) = 1 Dly 0.3       'バルブ2OFFをON
522     EndIf
523     '
524 '    Wait M_In(11276) = 1 Or M_In(11277) = 1     '回転センター検出
525     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    '回転センター検出
526     MRtn2 = frInCheck(11277,1,MSETTIMEOUT05&)
527 '
528     If MRtn = 1 Or MRtn2 = 1 Then GoTo *CompJigIni2
529     fErrorProcess(11,265,284,0)
530     If M_20# = MNext% Then M_20# = MClear%
531     If M_20# = MNgProcess% Then M_20# = MAbout%
532     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
533     If M_20# = MContinue% Then GoTo *RE_JIG_INI
534 *CompJigIni2
535     '
536     '
537     If M_In(11271) = 1 Then     '製品チャック閉ならば
538         M_Out(12259) = 0        '製品チャック閉OFF
539         M_Out(12258) = 1        '製品チャック開ON
540 '        Wait M_In(11271) = 1    '製品チャック開検出
541         M_Out(12263) = 0        '回転ストッパー戻OFF
542         M_Out(12262) = 1        '回転ストッパー出ON
543 '        Wait M_In(11274) = 1    '回転ストッパー出端検出
544     EndIf
545     '
546     M_Out(12261) = 0            '製品クランパー引OFF
547     M_Out(12260) = 1            '製品クランパー出ON
548 '    Wait M_In(11272) = 1        '製品クランパー出端検出
549     '
550     M_Out(12256) = 0            'チャック閉OFF
551     M_Out(12257) = 1            'チャック開ON
552 '    Wait M_In(11265)            'チャック開センサーON
553 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'チャック開センサーON
554 '    If MRtn = 0 Then
555 '        fErrorProcess()         'エラー処理
556 '    EndIf
557     '
558 ''    Mov PInitialPosition
559 MRtn = 1        'MRtn初期化
560 'チケットIDを読む
561 *RE_TICKET_READ
562 If M_20# = MContinue% Then M_20# = MClear%
563 'PInspPosition(1) = PTicketRead  'IDチケット読取位置
564 'MInspGroup%(1) = 1              '検査G番号
565 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
566 M_20# = MClear%                       '初期化
567 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
568     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
569     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
570     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
571 EndIf
572 If MRtn = 1 Then GoTo *CompRead
573 'fErrorProcess(11,214,251,0)
574 'If M_20# = MNext% Then M_20# = MClear%
575 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
576 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
577 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
578 If M_20# = MNext% Then M_20# = MPass%
579 Mvs PTicketRead_1                         '22/04/07 追加 渡辺
580 GoTo *ASSY_ERROR_END
581 *CompRead
582     fnAutoScreenComment(521)        '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
583     '
584     'パレットから製品を取る
585     M_Out(12269) = 0            '位置決め戻OFF
586     M_Out(12268) = 1            '位置決め出ON
587     Mov PProductOnPltGet_2      '製品上空回避点
588     '
589     *RE_PLT_GET
590     '
591     M_Out(12269) = 0            '位置決め戻OFF
592     M_Out(12268) = 1            '位置決め出ON
593     M_Out(12256) = 0            'チャック閉OFF
594     M_Out(12257) = 1            'チャック開ON
595 '
596 '    Wait M_In(11265)            'チャック開センサーON
597     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'チャック開センサーON
598     If MRtn = 1 Then GoTo *CompPltGet1
599     fErrorProcess(11,244,284,0)
600     If M_20# = MNext% Then M_20# = MClear%
601     If M_20# = MAbout% Or M_20# = MNgProcess% Then
602         Mov PInitialPosition    '退避ルート
603         Break
604     EndIf
605     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
606     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
607     If M_20# = MContinue% Then GoTo *RE_PLT_GET
608     *CompPltGet1
609 '
610 '    Mov PProductOnPltGet_1      '製品上空(位置変更1/14中村)
611 '
612 '    Wait M_In(11262) = 1        '位置決め出端センサーON
613     MRtn = frInCheck(11262,1,MSETTIMEOUT05&)   '位置決め出端センサーON
614     If MRtn = 1 Then GoTo *CompPltGet2
615     fErrorProcess(11,231,282,0)
616     If M_20# = MNext% Then M_20# = MClear%
617     If M_20# = MAbout% Or M_20# = MNgProcess% Then
618         M_Out(12268) = 0            '位置決め
619         M_Out(12269) = 1            '位置決め戻ON
620         Mov PProductOnPltGet_2
621         Mov PInitialPosition'退避ルート
622         Break
623     EndIf
624     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
625     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
626     If M_20# = MContinue% Then GoTo *RE_PLT_GET
627     *CompPltGet2
628 '
629     Mov PProductOnPltGet_1      '製品上空
630     M_Out(12268) = 0            '位置決め出OFF
631     M_Out(12269) = 1            '位置決め戻ON
632 '    Wait M_In(11263) = 1        '位置決め戻端センサーON
633     MRtn = frInCheck(11263,1,MSETTIMEOUT05&)   '位置決め戻り端センサー
634     If MRtn = 1 Then GoTo *CompPltGet3
635     fErrorProcess(11,234,284,0)
636     If M_20# = MNext% Then M_20# = MClear%
637     If M_20# = MAbout% Or M_20# = MNgProcess% Then
638         Mov PProductOnPltGet_2      '退避ルート
639         Mov PInitialPosition
640         Break
641     EndIf
642     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
643     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
644     If M_20# = MContinue% Then GoTo *RE_PLT_GET
645     *CompPltGet3
646 '
647     Ovrd 30
648     Mvs PProductOnPltGet        '製品を取る位置
649     Dly 0.1
650     M_Out(12257) = 0            'チャック開OFF
651     M_Out(12256) = 1            'チャック閉ON
652 '
653 '    Wait M_In(11266) = 1        'チャック閉センサーON
654     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'チャック閉センサーON
655     If MRtn = 1 Then GoTo *CompPltGet4
656     M_Out(12256) = 0        '退避ルート
657     M_Out(12257) = 1
658     Dly 2.0
659     Mvs PProductOnPltGet_1
660     Mov PProductOnPltGet_2
661     M_Out(12257) = 0
662     fErrorProcess(11,245,284,0)
663     If M_20# = MNext% Then M_20# = MClear%
664     If M_20# = MAbout% Or M_20# = MNgProcess% Then
665         Mov PInitialPosition
666         Break
667     EndIf
668     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
669     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
670     Mvs PProductOnPltGet_1
671     Mvs PProductOnPltGet
672     Dly 0.1
673     M_Out(12257) = 0            'チャック開OFF
674     M_Out(12256) = 1            'チャック閉ON
675     If M_20# = MContinue% Then GoTo *RE_PLT_GET
676     Mvs PProductOnPltGet
677     Dly 0.1
678     M_Out(12257) = 0            'チャック開OFF
679     M_Out(12256) = 1            'チャック閉ON
680     Dly 2.0
681     *CompPltGet4
682 '
683 '    Wait M_In(11264) = 1        '製品検出センサーON
684     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '製品検出センサーセンサーON
685     If MRtn = 1 Then GoTo *CompPltGet5
686     fErrorProcess(11,252,284,0)
687     If M_20# = MNext% Then M_20# = MClear%
688     If M_20# = MAbout% Or M_20# = MNgProcess% Then
689         M_Out(12256) = 0        '退避ルート
690         M_Out(12257) = 1
691         Mvs PProductOnPltGet_1
692         Mov PProductOnPltGet_2
693         Mov PInitialPosition
694         Break
695     EndIf
696     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
697     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
698     If M_20# = MContinue% Then GoTo *RE_PLT_GET
699     *CompPltGet5
700 '
701     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
702     Mvs PProductOnPltGet_1      '製品上空
703     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
704     Ovrd 60
705     Mov PProductOnPltGet_2      '製品上空回避点
706     '
707     '製品を治具に置く(点の追加,またそれによる変数名変更(9/2中村))
708     Fine 1.0 , P
709     Mov PProductOnJigSet_4      'パレット側回避点(末尾3から4(9/2中村))
710     Fine 0 , P
711     Mov PProductOnJigSet_3      'パレット-治具中間点(末尾2から3(9/2中村))
712     Mov PProductOnJigSet_2      '治具側回避点(点の追加(9/2中村))
713     '
714     *RE_JIG_SET_1
715     '
716     M_Out(12259) = 0            '治具製品チャック閉OFF
717     M_Out(12258) = 1            '治具製品チャック開ON
718     M_Out(12261)=0              '治具製品クランパー引込端OFF
719     M_Out(12260)=1              '治具製品クランパー出端ON
720 '
721 '    Wait M_In(11270) = 1        '治具製品チャック開センサーON
722     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   '治具製品チャック開センサーON
723     If MRtn = 1 Then GoTo *CompJIGSet1
724     fErrorProcess(11,259,284,0)
725     If M_20# = MNext% Then M_20# = MClear%
726     If M_20# = MAbout% Or M_20# = MNgProcess% Then
727         Mov PProductOnJigSet_2      '退避ルート
728         Mov PProductOnJigSet_3
729         Mov PProductOnJigSet_4
730         Mov PProductOnPltSet_2
731         Mov PProductOnPltSet_1
732         Ovrd 25
733         Mvs PProductOnPltSet
734         Dly 0.3
735         M_Out(12256)=0              'チャック閉OFF
736         M_Out(12257)=1              'チャック開ON
737         Dly 0.5
738         Mvs PProductOnPltSet_1
739         Ovrd 100
740         Mov PProductOnPltSet_2
741         Mov PInitialPosition
742         Break
743     EndIf
744     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
745     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
746     If M_20# = MContinue% Then GoTo *RE_JIG_SET_1
747     *CompJIGSet1
748 '
749 '    Wait M_In(11272) = 1        '製品クランパー出端センサーON
750     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '製品クランパー出端センサーON
751     If MRtn = 1 Then GoTo *CompJIGSet2
752     fErrorProcess(11,257,284,0)
753     If M_20# = MNext% Then M_20# = MClear%
754     If M_20# = MAbout% Or M_20# = MNgProcess% Then
755         Mov PProductOnJigSet_2      '退避ルート
756         Mov PProductOnJigSet_3
757         Mov PProductOnJigSet_4
758         Mov PProductOnPltSet_2
759         Mov PProductOnPltSet_1
760         Ovrd 25
761         Mvs PProductOnPltSet
762         Dly 0.3
763         M_Out(12256)=0              'チャック閉OFF
764         M_Out(12257)=1              'チャック開ON
765         Dly 2.0
766         Mvs PProductOnPltSet_1
767         Ovrd 100
768         Mov PProductOnPltSet_2
769         Mov PInitialPosition
770         Break
771     EndIf
772     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
773     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
774     If M_20# = MContinue% Then GoTo *RE_JIG_SET_1
775     *CompJIGSet2
776 '
777     Mov PProductOnJigSet_1      '治具上空
778     Ovrd 30
779     Mvs PProductOnJigSet        '製品置き位置
780     '
781 '
782 '2022.03.12 追加
783 '====================================================
784 '治具製品チャック開
785 *RE_JIG_SET_SP1
786     M_Out(12258) = 1            '治具製品チャック開ON
787     M_Out(12259) = 0            '治具製品チャック閉OFF
788 '
789     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)
790     If MRtn = 1 Then GoTo *CompJIGSetSP1
791     fErrorProcess(11,258,284,0)
792     If M_20# = MNext% Then M_20# = MClear%
793     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
794 '        M_Out(12256)=0              'チャック閉OFF
795 '        M_Out(12257)=1              'チャック開ON
796 '        Dly 2.0
797         Ovrd 5
798         Mvs PProductOnJigSet_1
799         Mov PProductOnJigSet_2
800         Mov PProductOnJigSet_3
801         Mov PProductOnJigSet_4
802         Mov PInitialPosition
803         Ovrd 30
804         Break
805     EndIf
806     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
807     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
808     If M_20# = MContinue% Then GoTo *RE_JIG_SET_SP1
809 *CompJIGSetSP1
810 '
811 '
812 '====================================================
813 'ハンド製品チャック開
814     M_Out(12256) = 0            'ハンド製品チャック閉OFF
815     M_Out(12257) = 1            'ハンド製品チャック開ON
816 '
817     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)
818     If MRtn = 1 Then GoTo *CompJIGSetSP2
819     fErrorProcess(11,258,284,0)
820     If M_20# = MNext% Then M_20# = MClear%
821     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
822 '        M_Out(12256)=0              'チャック閉OFF
823 '        M_Out(12257)=1              'チャック開ON
824         Dly 2.0
825         Ovrd 5
826         Mvs PProductOnJigSet_1
827         Mov PProductOnJigSet_2
828         Mov PProductOnJigSet_3
829         Mov PProductOnJigSet_4
830         Mov PInitialPosition
831         Ovrd 30
832         Break
833     EndIf
834     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
835     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
836     If M_20# = MContinue% Then GoTo *RE_JIG_SET_SP1
837 *CompJIGSetSP2
838 '
839 '
840 '====================================================
841 '治具製品チャック閉
842     M_Out(12258) = 0            '治具製品チャック開OFF
843     M_Out(12259) = 1            '治具製品チャック閉ON
844 '
845     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)
846     If MRtn = 1 Then GoTo *CompJIGSetSP3
847     fErrorProcess(11,258,284,0)
848     If M_20# = MNext% Then M_20# = MClear%
849     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
850         M_Out(12258)=1              'チャック開ON
851         M_Out(12259)=0              'チャック閉OFF
852         Dly 2.0
853         Ovrd 5
854         Mvs PProductOnJigSet_1
855         Mov PProductOnJigSet_2
856         Mov PProductOnJigSet_3
857         Mov PProductOnJigSet_4
858         Mov PInitialPosition
859         Ovrd 30
860         Break
861     EndIf
862     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
863     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
864     If M_20# = MContinue% Then GoTo *RE_JIG_SET_SP1
865 *CompJIGSetSP3
866 '
867 '
868 '====================================================
869 '治具製品クランパー引込、閉じる
870     M_Out(12260)=0              '製品クランパー出端OFF
871     M_Out(12261)=1              '製品クランパー引込端ON
872 '    Wait M_In(11273)=1          '製品クランパー引込端センサーON
873     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '治具製品クランパー引込端センサーON
874     If MRtn = 1 Then GoTo *CompJIGSetSP4
875     fErrorProcess(11,256,284,0)
876     If M_20# = MNext% Then M_20# = MClear%
877     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
878         M_Out(12256)=1              'チャック出ON
879         M_Out(12257)=0              'チャック引込OFF
880         Dly 2.0
881         Ovrd 5
882         Mvs PProductOnJigSet_1
883         Mov PProductOnJigSet_2
884         Mov PProductOnJigSet_3
885         Mov PProductOnJigSet_4
886         Mov PInitialPosition
887         Ovrd 30
888         Break
889     EndIf
890     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
891     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
892     If M_20# = MContinue% Then GoTo *RE_JIG_SET_SP1
893     *CompJIGSetSP4
894 '
895 '
896 '
897 '====================================================
898 '2022.03.12 順番入換の為削除
899 '
900 '以下は前の動き
901 '    *RE_JIG_SET_2
902 '    '
903 '    M_Out(12258) = 0            '治具製品チャック開OFF
904 '    M_Out(12259) = 1            '治具製品チャック閉ON
905 '    '
906 ''    Wait M_In(11271) = 1
907 '    MRtn = frInCheck(11271,1,MSETTIMEOUT05&)
908 '    If MRtn = 1 Then GoTo *CompJIGSet3
909 '    fErrorProcess(11,258,284,0)
910 '    If M_20# = MNext% Then M_20# = MClear%
911 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
912 '        M_Out(12256)=0              'チャック閉OFF
913 '        M_Out(12257)=1              'チャック開ON
914 '        Dly 2.0
915 '        Mvs PProductOnJigSet_1
916 '        Mov PProductOnJigSet_2
917 '        Mov PProductOnJigSet_3
918 '        Mov PProductOnJigSet_4
919 '        Mov PInitialPosition
920 '        Break
921 '    EndIf
922 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
923 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
924 '    If M_20# = MContinue% Then GoTo *RE_JIG_SET_2
925 '    *CompJIGSet3
926 '    '
927 '    M_Out(12260)=0              '製品クランパー出端OFF
928 '    M_Out(12261)=1              '製品クランパー引込端ON
929 ''    Wait M_In(11273)=1          '製品クランパー引込端センサーON
930 '    MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '治具製品クランパー引込端センサーON
931 '    If MRtn = 1 Then GoTo *CompJIGSet4
932 '    fErrorProcess(11,256,284,0)
933 '    If M_20# = MNext% Then M_20# = MClear%
934 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
935 '        M_Out(12256)=0              'チャック閉OFF
936 '        M_Out(12257)=1              'チャック開ON
937 '        Dly 2.0
938 '        Mvs PProductOnJigSet_1
939 '        Mov PProductOnJigSet_2
940 '        Mov PProductOnJigSet_3
941 '        Mov PProductOnJigSet_4
942 '        Mov PInitialPosition
943 '        Break
944 '    EndIf
945 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
946 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
947 '    If M_20# = MContinue% Then GoTo *RE_JIG_SET_2
948 '    *CompJIGSet4
949 '    '
950 '    M_Out(12256)=0              'チャック閉OFF
951 '    M_Out(12257)=1              'チャック開ON
952 ''    Wait M_In(11265)=1          'チャック開センサーON
953 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'チャック開センサーON
954 '    If MRtn = 1 Then GoTo *CompJIGSet5
955 '    fErrorProcess(11,244,284,0)
956 '    If M_20# = MNext% Then M_20# = MClear%
957 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
958 '        Mvs PProductOnJigSet_1
959 '        Mov PProductOnJigSet_2
960 '        Mov PProductOnJigSet_3
961 '        Mov PProductOnJigSet_4
962 '        Mov PInitialPosition
963 '        Break
964 '    EndIf
965 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
966 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
967 '    If M_20# = MContinue% Then GoTo *RE_JIG_SET_2
968 '    *CompJIGSet5
969 '2022.03.12 順番入換の為削除
970 '====================================================
971 '
972 '
973     '
974     Mvs PProductOnJigSet_1      '治具上空
975     Mvs PProductOnJigSet_2      '治具側回避点(点の追加(9／2中村))
976     '
977     *RE_JIG_SET_3
978     '
979 '    Wait M_In(11264)=0          '製品検出センサーOFF
980     MRtn = frInCheck(11264,0,MSETTIMEOUT05&)   '製品検出センサーOFF
981     If MRtn = 1 Then GoTo *CompJIGSet6
982     fErrorProcess(11,253,284,0)
983     If M_20# = MNext% Then M_20# = MClear%
984     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
985         Mvs PProductOnJigSet_1
986         Mov PProductOnJigSet_2
987         Mov PProductOnJigSet_3
988         Mov PProductOnJigSet_4
989         Mov PInitialPosition
990         Break
991     EndIf
992     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
993     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
994     If M_20# = MContinue% Then GoTo *RE_JIG_SET_3
995     *CompJIGSet6
996     '
997     Ovrd 100
998     '
999 'ねじ締め順変更ここから(6/9中村)
1000     *RE_SCREW_SET_1
1001     '
1002     M_Out(12257) = 0            'チャック開OFF(ネジピックアップ時干渉対策9/10中村)
1003     M_Out(12256) = 1            'チャック閉ON(ネジピックアップ時干渉対策9/10中村)
1004 '    Wait M_In(11266) = 1        'チャック閉センサーON
1005     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    'チャック閉センサーON
1006     If MRtn = 1 Then GoTo *CompScrewSet1
1007     fErrorProcess(11,245,284,0)
1008     If M_20# = MNext% Then M_20# = MClear%
1009     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1010         Mvs PProductOnJigSet_1
1011         Mvs PProductOnJigSet_2
1012         Mov PProductOnJigSet_3
1013         Mov PProductOnJigSet_4
1014         Mov PInitialPosition
1015         Break
1016     EndIf
1017     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1018     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1019     If M_20# = MContinue% Then GoTo *RE_SCREW_SET_1
1020     *CompScrewSet1
1021 'ハンドの向きをHSねじ締めの向きに変更する座標が必要(6/9中村)
1022     Mov PScrewSupplyHS_5        'ハンド向き変え
1023     Mov PScrewSupplyHS_3        '治具側ねじロボ回避点
1024     'ヒートシンクのネジ締め
1025     '
1026     'ヒートシンク用ネジ供給機へネジを取りに行く
1027     'GoSub *ScrewSupplyHS         'コメントアウト(9/3中村)
1028     '*ScrewSupplyHS               'コメントアウト
1029     PGetScrewPos(1) = PScrewSupplyHS_1  'ネジピックアップ上空
1030     PGetScrewPos(2) = PScrewSupplyHS_2  'ネジ供給機回避点
1031     PGetScrewPos(9) = PScrewSupplyHS_4  '吸着不良捨て位置
1032     PGetScrewPos(10) = PScrewSupplyHS   'ネジピックアップ位置
1033 '    Mov PScrewSupplyHS_2          'ネジ供給機回避点(9/30以下5行コメントアウト(中村))
1034 '    Mvs PScrewSupplyHS_1          'ネジピックアップ上空(MovからMvsへ変更(9/3中村))
1035 '    Mvs PScrewSupplyHS            'ネジピックアップ
1036 '    Mvs PScrewSupplyHS_1          'ネジピックアップ上空
1037 '    Mvs PScrewSupplyHS_2          'ネジ供給機回避点(MovからMvsへ変更(9/3中村))
1038     *RE_SCREW_GET_1
1039     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
1040     If MRtn = 1 Then GoTo *CompScrewGet1
1041     If M_20# = MNext% Then M_20# = MClear%
1042     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1043         Mov PEscapePosition_4
1044         Mov PEscapePosition_2
1045         Mov PEscapePosition
1046         Mov PInitialPosition
1047         Break
1048     EndIf
1049     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1050     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1051     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
1052     *CompScrewGet1
1053     '
1054     Mov PScrewSupplyHS_3          '治具側ねじロボ回避点
1055     'Return                       'コメントアウト(9/3中村)
1056     '
1057     '?@番ネジ締め
1058 '    Mov PScrewHeatSink1_1         '?@上空(以下5行コメントアウト(9/30中村))
1059 '    Ovrd 5
1060 '    Mvs PScrewHeatSink1           '?@ネジ着座
1061 '    Ovrd 10
1062 '    Mvs PScrewHeatSink1_1         '?@上空
1063     PScrewPos(1) = PScrewHeatSink1_1    'ねじ締め上空(以下4行追加(9/30中村))
1064     PScrewPos(2) = PScrewHeatSink1_0    'ねじ締め開始位置
1065     PScrewPos(10) = PScrewHeatSink1     'ねじ締め終了位置
1066     M_Out16(12672) = 1              'ネジ締め位置番号送信
1067     MRtn = ScrewTight(PScrewPos,2,4.914)          'ねじ締め開始
1068     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1069     If MRtn = 1 Then GoTo *CompScrew1
1070     Mov PScrewSupplyHS_3
1071     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1072     MScrewErrorCord% = MScrewErrorCord% + 1
1073     fErrorProcess(11,MScrewErrorCord%,52,0)
1074 '    fErrorProcess(11,53,52,0)
1075     If M_20# = MNext% Then M_20# = MClear%
1076     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1077         Mov PEscapePosition_3
1078         Mov PEscapePosition_2
1079         Mov PEscapePosition
1080         Mov PInitialPosition
1081         Break
1082     EndIf
1083     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1084     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1085     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
1086     *CompScrew1
1087     '
1088     'ヒートシンク用ネジ供給機へネジを取りに行く
1089     'GoSub *ScrewSupplyHS         'コメントアウト(9/10中村)
1090     Mov PScrewSupplyHS_3          '治具側ねじロボ回避点(以下3行追加(9/30中村))
1091     *RE_SCREW_GET_2
1092     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
1093     If MRtn = 1 Then GoTo *CompScrewGet2
1094     If M_20# = MNext% Then M_20# = MClear%
1095     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1096         Mov PEscapePosition_4
1097         Mov PEscapePosition_2
1098         Mov PEscapePosition
1099         Mov PInitialPosition
1100         Break
1101     EndIf
1102     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1103     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1104     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
1105     *CompScrewGet2
1106     '
1107     Mov PScrewSupplyHS_3          '治具側ねじロボ回避点
1108     '
1109     '?A番ネジ締め
1110 '    Mov PScrewHeatSink2_1         '?A上空(以下5行コメントアウト(9/30中村))
1111 '    Ovrd 5
1112 '    Mvs PScrewHeatSink2           '?Aネジ着座
1113 '    Ovrd 10
1114 '    Mvs PScrewHeatSink2_1         '?A上空
1115     PScrewPos(1) = PScrewHeatSink2_1    'ねじ締め上空(以下4行追加(9/30中村))
1116     PScrewPos(2) = PScrewHeatSink2_0    'ねじ締め開始位置
1117     PScrewPos(10) = PScrewHeatSink2     'ねじ締め終了位置
1118     M_Out16(12672) = 2              'ネジ締め位置番号送信
1119     MRtn = ScrewTight(PScrewPos,6,4.914)          'ねじ締め開始
1120     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1121     If MRtn = 1 Then GoTo *CompScrew2
1122     Mov PScrewSupplyHS_3
1123     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1124     MScrewErrorCord% = MScrewErrorCord% + 2
1125     fErrorProcess(11,MScrewErrorCord%,52,0)
1126 '    fErrorProcess(11,54,52,0)
1127     If M_20# = MNext% Then M_20# = MClear%
1128     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1129         Mov PEscapePosition_3
1130         Mov PEscapePosition_2
1131         Mov PEscapePosition
1132         Mov PInitialPosition
1133         Break
1134     EndIf
1135     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1136     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1137     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
1138     *CompScrew2
1139     '
1140     '
1141     '
1142     '
1143     Mov PScrewSupplyPlate_3          'ネジピックアップ向き合わせ位置
1144     '
1145 '    *RE_SCREW_SET_2
1146     '
1147     '治具左上向きにて90度回転(並列処理化4/13中村)
1148 '    M_Out(12262) = 0             '回転ストッパー出OFF
1149 '    M_Out(12263) = 1             '回転ストッパー戻ON
1150 '    '
1151 ''    Wait M_In(11275) = 1         '回転ストッパー戻端検出センサーON
1152 '    MRtn = frInCheck(11275,1,MSETTIMEOUT05&)   '位置決め戻端センサーON
1153 '    If MRtn = 1 Then GoTo *CompScrewSet2
1154 '    fErrorProcess(11,262,284,0)
1155 '    If M_20# = MNext% Then M_20# = MClear%
1156 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1157 '        Mov PEscapePosition_3
1158 '        Mov PEscapePosition_2
1159 '        Mov PEscapePosition
1160 '        Mov PInitialPosition
1161 '        Break
1162 '    EndIf
1163 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1164 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1165 '    If M_20# = MContinue% Then GoTo *RE_SCREW_SET_2
1166 '    *CompScrewSet2
1167     M_25# = 1                   '並列処理：CW回転開始(デバッグ中)
1168 '
1169     '左側面のネジ締め
1170     '
1171     '側板用ネジ供給機へネジを取りに行く
1172     'GoSub *ScrewSupplyPlate
1173     '*ScrewSupplyPlate
1174     PGetScrewPos(1) = PScrewSupplyPlate_1  'ネジピックアップ上空
1175     PGetScrewPos(2) = PScrewSupplyPlate_2  'ネジ供給機回避点
1176     PGetScrewPos(9) = PScrewSupplyPlate_7  '吸着不良戻し位置
1177     PGetScrewPos(10) = PScrewSupplyPlate   'ネジピックアップ位置
1178 *RE_SCREW_GET_3
1179     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        'ネジ受け取り開始
1180 '
1181     If MRtn = 1 Then GoTo *CompScrewGet3
1182     If M_20# = MNext% Then M_20# = MClear%
1183     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1184         Mov PEscapePosition_3
1185         Mov PEscapePosition_2
1186         Mov PEscapePosition
1187         Mov PInitialPosition
1188         Break
1189     EndIf
1190     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1191     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1192     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
1193     *CompScrewGet3
1194     '
1195     Mov PScrewSupplyPlate_3          'ネジ供給機-治具間中間点(追加9/10中村)
1196     'タクト短縮のため追加、修正2/3中村
1197 '    *RE_CW_ROT
1198 '    If M_20# = MContinue% Then
1199 '        M_Out(12264) = 1 Dly 0.3     'CW1バルブON
1200 '    EndIf
1201 '    MRtn = frInCheck(11278,1,MSETTIMEOUT05&)   'CW端センサーON
1202 '    If MRtn = 1 Then GoTo *CompScrewSet3
1203 '    fErrorProcess(11,264,284,0)
1204 '    If M_20# = MNext% Then M_20# = MClear%
1205 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1206     M_20# = MClear%
1207 *LOOP1
1208     If M_20# = MContinue% Then
1209         M_25# = 1
1210         M_20# = MClear%
1211     EndIf
1212     If M_26# <> 0 Then GoTo *LOOP1END Else GoTo *LOOP1
1213 *LOOP1END
1214 '
1215     If M_26# = 1 Then GoTo *Set1End                            '正常終了ならエラー処理なし
1216     If M_In(11275) = 1 And M_In(11278) = 1 Then GoTo *Set1End  '今現在が正常終了と同じセンサー状態ならエラー処理なし
1217     If M_In(11275) = 0 Then                                    'センサーの状態を見てエラーを出す
1218         fErrorProcess(11,262,284,0)
1219     ElseIf M_In(11278) = 0 Then
1220         fErrorProcess(11,264,284,0)
1221     EndIf
1222     M_26# = 0
1223     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1224         Mov PEscapePosition_3
1225         Mov PScrewSupplyPlate_7  '吸着不良戻し位置
1226         M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1227         Dly 0.2
1228         '破壊ON
1229         M_Out(Y6B_VB1)=1 '真空破壊ON
1230         'ビット回転
1231         M_Out(Y61_Driver)=1
1232         Dly 0.5
1233         '                '
1234         Ovrd 100
1235         JOvrd M_NJovrd
1236         Spd M_NSpd
1237         'ドライバーを上下させねじを振り落とす
1238         Mov PScrewSupplyPlate_7,10
1239         Mov PScrewSupplyPlate_7
1240         Dly 0.1
1241         Mov PScrewSupplyPlate_7,10
1242         Mov PScrewSupplyPlate_7
1243 '
1244         'ネジ落ち待ち
1245         Wait M_In(11268) = 0
1246         'ビット回転停止
1247         M_Out(Y61_Driver)=0
1248         Dly 0.1
1249         '破壊OFF
1250         M_Out(Y6B_VB1)=0 '真空破壊OFF
1251         Mov PEscapePosition_2
1252         Mov PEscapePosition
1253         Mov PInitialPosition
1254         Break
1255     EndIf
1256     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1257     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1258 '    If M_20# = MContinue% Then GoTo *RE_CW_ROT
1259 '    *CompScrewSet3
1260     If M_20# = MNext% Then M_20# = MClear%
1261     If M_20# = MContinue% Then GoTo *LOOP1
1262 '
1263 *Set1End
1264 '    M_26# = 0                       '初期化タイミング変更6/9中村
1265 '
1266     Mov PScrewSupplyPlate_4          '治具側回避点
1267     'Return                          'コメントアウト(9/3中村)
1268     '
1269     '側板Lネジ締め(仕様変更につきPScrewPlateL1をPScrewPlateLへ変更)
1270 '    Mov PScrewPlateL1_1            '?@上空(以下5行コメントアウト(9/30中村))
1271 '    Ovrd 5
1272 '    Mvs PScrewPlateL1              '?@ネジ着座
1273 '    Ovrd 10
1274 '    Mvs PScrewPlateL1_1            '?@上空
1275     PScrewPos(1) = PScrewPlateL1_1    'ねじ締め上空(以下4行追加(9/30中村))
1276     PScrewPos(2) = PScrewPlateL1_0    'ねじ締め開始位置
1277     PScrewPos(10) = PScrewPlateL1     'ねじ締め終了位置
1278     M_Out16(12672) = 3              'ネジ締め位置番号送信
1279     MRtn = ScrewTight(PScrewPos,4,4.914)          'ねじ締め開始
1280     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1281     If MRtn = 1 Then GoTo *CompScrew3
1282     Mov PScrewSupplyPlate_4
1283     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1284     MScrewErrorCord% = MScrewErrorCord% + 3
1285     fErrorProcess(11,MScrewErrorCord%,52,0)
1286 '    fErrorProcess(11,55,52,0)
1287     If M_20# = MNext% Then M_20# = MClear%
1288     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1289         Mov PEscapePosition_3
1290         Mov PEscapePosition_2
1291         Mov PEscapePosition
1292         Mov PInitialPosition
1293         Break
1294     EndIf
1295     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1296     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1297     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
1298     *CompScrew3
1299     M_26# = 0                       '初期化タイミング変更6/9中村
1300 '
1301 '    '以下17行仕様変更につきコメントアウト(11/4中村)
1302 '    '側板用ネジ供給機へネジを取りに行く
1303 '    'GoSub *ScrewSupplyPlate       'コメントアウト(9/9中村)
1304     Mov PScrewSupplyPlate_4          '治具側回避点(以下5行追加(9/30中村))
1305     Mov PScrewSupplyPlate_3          '治具-ネジ供給機間中間点
1306     *RE_SCREW_GET_4
1307     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        'ネジ受け取り開始
1308     If MRtn = 1 Then GoTo *CompScrewGet4
1309     If M_20# = MNext% Then M_20# = MClear%
1310     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1311         Mov PEscapePosition_3
1312         Mov PEscapePosition_2
1313         Mov PEscapePosition
1314         Mov PInitialPosition
1315         Break
1316     EndIf
1317     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1318     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1319     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
1320     *CompScrewGet4
1321     '
1322     Mov PScrewSupplyPlate_3          'ネジ供給機-治具間中間点
1323     Mov PScrewSupplyPlate_4          '治具側回避点
1324 '    '
1325 '    '?A番ネジ締め
1326 ''    Mov PScrewPlateL2_1            '?A上空(以下5行コメントアウト(9/30中村))
1327 ''    Ovrd 5
1328 ''    Mvs PScrewPlateL2              '?Aネジ着座
1329 ''    Ovrd 10
1330 ''    Mvs PScrewPlateL2_1            '?A上空
1331     PScrewPos(1) = PScrewPlateL2_1    'ねじ締め上空(以下4行追加(9/30中村))
1332     PScrewPos(2) = PScrewPlateL2_0    'ねじ締め開始位置
1333     PScrewPos(10) = PScrewPlateL2     'ねじ締め終了位置
1334     M_Out16(12672) = 4              'ネジ締め位置番号送信
1335     MRtn = ScrewTight(PScrewPos,4,4.914)          'ねじ締め開始
1336     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1337     If MRtn = 1 Then GoTo *CompScrew4
1338     Mov PScrewSupplyPlate_4
1339     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1340     MScrewErrorCord% = MScrewErrorCord% + 4
1341     fErrorProcess(11,MScrewErrorCord%,52,0)
1342 '    fErrorProcess(11,56,52,0)
1343     If M_20# = MNext% Then M_20# = MClear%
1344     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1345         Mov PEscapePosition_3
1346         Mov PEscapePosition_2
1347         Mov PEscapePosition
1348         Mov PInitialPosition
1349         Break
1350     EndIf
1351     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1352     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1353     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
1354     *CompScrew4
1355 '
1356     Mov PScrewSupplyPlate_4        '治具回転につきあらかじめ回避させる(追加9/10中村)
1357     '
1358     '
1359 '    *RE_SCREW_SET_3
1360 '    '
1361 '    M_Out(12265) = 1 Dly 0.3       'CW2バルブON
1362 ''    Wait M_In(11277) = 1           'CCWセンター検出センサーON
1363 '    MRtn = frInCheck(11277,1,MSETTIMEOUT05&)
1364 '    If MRtn = 1 Then GoTo *CompScrewSet4
1365 '    fErrorProcess(11,265,284,0)
1366 '    If M_20# = MNext% Then M_20# = MClear%
1367 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1368 '        Mov PEscapePosition_3
1369 '        Mov PEscapePosition_2
1370 '        Mov PEscapePosition
1371 '        Mov PInitialPosition
1372 '        Break
1373 '    EndIf
1374 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1375 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1376 '    If M_20# = MContinue% Then GoTo *RE_SCREW_SET_3
1377 '    *CompScrewSet4
1378 '    '
1379 '    *RE_SCREW_SET_4
1380 '    '
1381 '    M_Out(12266) = 1 Dly 0.3       'CCW1バルブON
1382 '    Wait M_In(11279) = 1           'CCW端検出センサーON
1383 '    MRtn = frInCheck(11279,1,MSETTIMEOUT05&)   'CCW端検出センサーON(タクト短縮のためコメントアウト2/3中村)
1384 '    If MRtn = 1 Then GoTo *CompScrewSet5
1385 '    fErrorProcess(11,266,284,0)
1386 '    If M_20# = MNext% Then M_20# = MClear%
1387 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1388 '        Mov PEscapePosition_3
1389 '        Mov PEscapePosition_2
1390 '        Mov PEscapePosition
1391 '        Mov PInitialPosition
1392 '        Break
1393 '    EndIf
1394 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1395 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1396 '    If M_20# = MContinue% Then GoTo *RE_SCREW_SET_4
1397 '    *CompScrewSet5
1398     '
1399     '右側面のネジ締め
1400     '
1401     '側板用ネジ供給機へネジを取りに行く
1402     'GoSub *ScrewSupplyPlate       'コメントアウト(9/9中村)
1403     Mov PScrewSupplyPlate_3          '治具-ネジ供給機間中間点(以下4行追加(9/30中村))
1404     '治具180度回転
1405     M_25# = 2
1406     *RE_SCREW_GET_5
1407     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        'ネジ受け取り開始
1408     If MRtn = 1 Then GoTo *CompScrewGet5
1409     If M_20# = MNext% Then M_20# = MClear%
1410     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1411         Mov PEscapePosition_3
1412         Mov PEscapePosition_2
1413         Mov PEscapePosition
1414         Mov PInitialPosition
1415         Break
1416     EndIf
1417     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1418     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1419     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
1420     *CompScrewGet5
1421     Mov PScrewSupplyPlate_3          'ネジ供給機-治具間中間点
1422 '
1423 '    *RE_CCW_ROT
1424 '    If M_20# = MContinue% Then
1425 '        M_Out(12266) = 1 Dly 0.3     'CW1バルブON
1426 '    EndIf
1427 '    'CCWセンサー検出(タクト短縮のため移動、修正）
1428 '    MRtn = frInCheck(11279,1,MSETTIMEOUT05&)   'CCW端検出センサーON
1429 '    If MRtn = 1 Then GoTo *CompScrewSet5
1430 '    fErrorProcess(11,266,284,0)
1431 '    If M_20# = MNext% Then M_20# = MClear%
1432     M_20# = MClear%
1433 *LOOP2
1434     If M_20# = MContinue% Then
1435         M_25# = 2
1436         M_20# = MClear%
1437     EndIf
1438     If M_26# <> 0 Then GoTo *LOOP2END Else GoTo *LOOP2
1439 *LOOP2END
1440 '
1441     If M_26# = 1 Then GoTo *Set2End                            '正常終了ならエラー処理なし
1442     If M_In(11279) = 1 Then GoTo *Set2End  '今現在が正常終了と同じセンサー状態ならエラー処理なし
1443     If M_In(11277) = 0 Then                                    'センサーの状態を見てエラーを出す
1444         fErrorProcess(11,265,284,0)
1445     ElseIf M_In(11279) = 0 Then
1446         fErrorProcess(11,266,284,0)
1447     EndIf
1448     M_26# = 0
1449     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1450         Mov PEscapePosition_3
1451         Mov PScrewSupplyPlate_7  '吸着不良戻し位置
1452         M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1453         Dly 0.2
1454         '破壊ON
1455         M_Out(Y6B_VB1)=1 '真空破壊ON
1456         'ビット回転
1457         M_Out(Y61_Driver)=1
1458         Dly 0.5
1459         '                '
1460         Ovrd 100
1461         JOvrd M_NJovrd
1462         Spd M_NSpd
1463         'ドライバーを上下させねじを振り落とす
1464         Mov PScrewSupplyPlate_7,10
1465         Mov PScrewSupplyPlate_7
1466         Dly 0.1
1467         Mov PScrewSupplyPlate_7,10
1468         Mov PScrewSupplyPlate_7
1469 '
1470         'ネジ落ち待ち
1471         Wait M_In(11268) = 0
1472         'ビット回転停止
1473         M_Out(Y61_Driver)=0
1474         Dly 0.1
1475         '破壊OFF
1476         M_Out(Y6B_VB1)=0 '真空破壊OFF
1477         Mov PEscapePosition_2
1478         Mov PEscapePosition
1479         Mov PInitialPosition
1480         Break
1481     EndIf
1482     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1483     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1484 '    If M_20# = MContinue% Then GoTo *RE_CCW_ROT
1485     If M_20# = MNext% Then M_20# = MClear%
1486     If M_20# = MContinue% Then GoTo *LOOP2
1487 '    *CompScrewSet5
1488 *Set2End
1489 '    M_26# = 0                       '初期化タイミング変更6/9中村
1490 '
1491     Mov PScrewSupplyPlate_4          '治具側回避点
1492     '
1493     '?@番ネジ締め(仕様変更につきPScrewPlateR1をPScrewPlateRへ変更)
1494 '    Mov PScrewPlateR1_1            '?@上空(以下5行コメントアウト(9/30中村))
1495 '    Ovrd 5
1496 '    Mvs PScrewPlateR1              '?@ネジ着座
1497 '    Ovrd 10
1498 '    Mvs PScrewPlateR1_1            '?@上空
1499     PScrewPos(1) = PScrewPlateR1_1    'ねじ締め上空(以下4行追加(9/30中村))
1500     PScrewPos(2) = PScrewPlateR1_0    'ねじ締め開始位置
1501     PScrewPos(10) = PScrewPlateR1     'ねじ締め終了位置
1502     M_Out16(12672) = 5              'ネジ締め位置番号送信
1503     MRtn = ScrewTight(PScrewPos,4,4.914)          'ねじ締め開始
1504     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1505     If MRtn = 1 Then GoTo *CompScrew5
1506     Mov PScrewSupplyPlate_4
1507     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1508     MScrewErrorCord% = MScrewErrorCord% + 5
1509     fErrorProcess(11,MScrewErrorCord%,52,0)
1510 '    fErrorProcess(11,57,52,0)
1511     If M_20# = MNext% Then M_20# = MClear%
1512     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1513         Mov PEscapePosition_3
1514         Mov PEscapePosition_2
1515         Mov PEscapePosition
1516         Mov PInitialPosition
1517         Break
1518     EndIf
1519     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1520     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1521     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
1522     *CompScrew5
1523     M_26# = 0                       '初期化タイミング変更6/9中村
1524 '
1525 '
1526     '以下18行仕様変更につきコメントアウト(11/4中村)
1527 '    '側板用ネジ供給機へネジを取りに行く
1528 '    'GoSub *ScrewSupplyPlate       'コメントアウト(9/9中村)
1529     Mov PScrewSupplyPlate_4          '治具側回避点(以下5行追加(9/30中村))
1530     Mov PScrewSupplyPlate_3          '治具-ネジ供給機間中間点
1531     '
1532     *RE_SCREW_GET_6
1533     '
1534     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        'ネジ受け取り開始
1535     If MRtn = 1 Then GoTo *CompScrewGet6
1536     If M_20# = MNext% Then M_20# = MClear%
1537     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1538         Mov PEscapePosition_3
1539         Mov PEscapePosition_2
1540         Mov PEscapePosition
1541         Mov PInitialPosition
1542         Break
1543     EndIf
1544     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1545     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1546     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
1547     *CompScrewGet6
1548     Mov PScrewSupplyPlate_3          'ネジ供給機-治具間中間点
1549     Mov PScrewSupplyPlate_4          '治具側回避点
1550 '    '
1551 '    '?A番ネジ締め
1552 ''    Mov PScrewPlateR2_1            '?A上空(以下5行コメントアウト(9/30中村))
1553 ''    Ovrd 5
1554 ''    Mvs PScrewPlateR2              '?Aネジ着座
1555 ''    Ovrd 10
1556 ''    Mvs PScrewPlateR2_1            '?A上空
1557     PScrewPos(1) = PScrewPlateR2_1    'ねじ締め上空(以下4行追加(9/30中村))
1558     PScrewPos(2) = PScrewPlateR2_0    'ねじ締め開始位置
1559     PScrewPos(10) = PScrewPlateR2     'ねじ締め終了位置
1560     M_Out16(12672) = 6              'ネジ締め位置番号送信
1561     MRtn = ScrewTight(PScrewPos,4,4.914)          'ねじ締め開始
1562     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1563     If MRtn = 1 Then GoTo *CompScrew6
1564     Mov PScrewSupplyPlate_4
1565     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1566     MScrewErrorCord% = MScrewErrorCord% + 6
1567     fErrorProcess(11,MScrewErrorCord%,52,0)
1568 '    fErrorProcess(11,58,52,0)
1569     If M_20# = MNext% Then M_20# = MClear%
1570     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1571         Mov PEscapePosition_3
1572         Mov PEscapePosition_2
1573         Mov PEscapePosition
1574         Mov PInitialPosition
1575         Break
1576     EndIf
1577     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1578     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1579     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
1580     *CompScrew6
1581     '
1582     Mov PScrewSupplyHS_3           '治具側ねじロボ回避点
1583     '
1584     '治具90度回転
1585 '    *RE_CENTER_POS
1586 '    M_Out(12267) = 1 Dly 0.3       'CCW2バルブON
1587 ''    Wait M_In(11276) = 1           'CWセンター検出
1588 '    MRtn = frInCheck(11276,1,MSETTIMEOUT05&)   'CWセンター検出
1589 '    If MRtn = 1 Then GoTo *CompSenterPos1
1590 '    fErrorProcess(11,265,284,0)
1591 '    If M_20# = MNext% Then M_20# = MClear%
1592 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1593 '        Mov PEscapePosition_2
1594 '        Mov PEscapePosition
1595 '        Mov PInitialPosition
1596 '        Break
1597 '    EndIf
1598 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1599 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1600 '    If M_20# = MContinue% Then GoTo *RE_CENTER_POS
1601 '    *CompSenterPos1
1602 '    '
1603 '    M_Out(12263) = 0               '回転ストッパー戻OFF
1604 '    M_Out(12262) = 1               '回転ストッパー出ON
1605 '    '
1606 ''    Wait M_In(11274) = 1           '回転ストッパー出端検出ON
1607 '    MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '回転ストッパー出端検出ON
1608 '    If MRtn = 1 Then GoTo *CompSenterPos2
1609 '    fErrorProcess(11,263,284,0)
1610 '    If M_20# = MNext% Then M_20# = MClear%
1611 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1612 '        Mov PEscapePosition_2
1613 '        Mov PEscapePosition
1614 '        Mov PInitialPosition
1615 '        Break
1616 '    EndIf
1617 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1618 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1619 '    If M_20# = MContinue% Then GoTo *RE_CENTER_POS
1620 '    *CompSenterPos2
1621 '    '
1622 '    '
1623 '    '治具から製品を取り出す
1624 '    '
1625 ''    Mov PProductOnJigGet_6          'ネジピックアップ向き合わせ位置
1626     M_25# = 3       '位置暫定
1627     Mov PProductOnJigGet_5          '製品チャック-ネジピックアップ向き合わせ中間位置
1628 '
1629     Mov PProductOnJigGet_4          '製品ピックアップ向き合わせ位置
1630     M_20# = MClear%
1631 *LOOP3
1632     If M_20# = MContinue% Then
1633         M_25# = 3
1634         M_20# = MClear%
1635     EndIf
1636     If M_26# <> 0 Then GoTo *LOOP3END Else GoTo *LOOP3
1637 *LOOP3END
1638     If M_26# = 1 Then GoTo *Set3End                            '正常終了ならエラー処理なし
1639     If M_In(11274) = 1 And M_In(11276) = 1 Then GoTo *Set3End  '今現在が正常終了と同じセンサー状態ならエラー処理なし
1640     If M_In(11276) = 0 Then                                    'センサーの状態を見てエラーを出す
1641         fErrorProcess(11,265,284,0)
1642     ElseIf M_In(11274) = 0 Then
1643         fErrorProcess(11,263,284,0)
1644     EndIf
1645     M_26# = 0
1646     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1647         Mov PProductOnJigGet_4
1648         Mov PProductOnJigGet_3
1649         Mov PProductOnPltSet_3
1650         Mov PProductOnPltSet_2
1651         Mov PInitialPosition
1652         Break
1653     EndIf
1654     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1655     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1656 '    If M_20# = MContinue% Then GoTo *RE_CCW_ROT
1657     If M_20# = MNext% Then M_20# = MClear%
1658     If M_20# = MContinue% Then GoTo *LOOP3
1659 '    *CompScrewSet5
1660 *Set3End
1661     M_26# = 0
1662 'ねじ締め順変更ここまで(6/9中村)
1663     *RE_JIG_GET_1
1664     '
1665     M_Out(12259) = 0                '治具製品チャック閉OFF
1666     M_Out(12258) = 1                '治具製品チャック開ON
1667     M_Out(12261) = 0                '製品クランパー引込端OFF
1668     M_Out(12260) = 1                '製品クランパー出端ON
1669     M_Out(12256)= 0                 '製品チャック閉OFF
1670     M_Out(12257)= 1                 '製品チャック開ON
1671     '
1672 '    Wait M_In(11265)=1              '製品チャック開センサーON
1673     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '製品チャック開センサーON
1674     If MRtn = 1 Then GoTo *CompJigGet1
1675     fErrorProcess(11,244,284,0)
1676     If M_20# = MNext% Then M_20# = MClear%
1677     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1678         Mov PProductOnJigGet_3
1679         Mov PProductOnPltSet_3
1680         Mov PProductOnPltSet_2
1681         Mov PInitialPosition
1682         Break
1683     EndIf
1684     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1685     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1686     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1687     *CompJigGet1
1688     '
1689 '    Wait M_In(11272)=1              '治具製品クランパーセンサー開放端ON
1690     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '治具製品クランパーセンサー解放端ON
1691     If MRtn = 1 Then GoTo *CompJigGet2
1692     fErrorProcess(11,257,284,0)
1693     If M_20# = MNext% Then M_20# = MClear%
1694     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1695         Mov PProductOnJigGet_3
1696         Mov PProductOnPltSet_3
1697         Mov PProductOnPltSet_2
1698         Mov PInitialPosition
1699         Break
1700     EndIf
1701     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1702     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1703     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1704     *CompJigGet2
1705     '
1706     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)
1707     If MRtn = 1 Then GoTo *CompJigGet3
1708     fErrorProcess(11,259,284,0)
1709     If M_20# = MNext% Then M_20# = MClear%
1710     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1711         Mov PProductOnJigGet_3
1712         Mov PProductOnPltSet_3
1713         Mov PProductOnPltSet_2
1714         Mov PInitialPosition
1715         Break
1716     EndIf
1717     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1718     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1719     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1720     *CompJigGet3
1721     '
1722     Ovrd 100
1723     'Mov PProductOnJigGet_3          'ハンド手首回転(コメントアウト9/10中村)
1724     Mov PProductOnJigGet_2          '治具上空回避点
1725     Mvs PProductOnJigGet_1          '治具上空
1726     Ovrd 30
1727     Mvs PProductOnJigGet            '製品取り出し位置
1728     *RETRY_PRODUCT_ON_JIG_GET_2
1729     M_Out(12257)=0                  '製品チャック開OFF
1730     M_Out(12256)=1                  '製品チャック閉ON
1731 '    Wait M_In(11266)=1              '製品チャック閉センサーON
1732     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '製品チャック閉センサーON
1733     If MRtn = 1 Then GoTo *CompJigGet4
1734 '
1735     fErrorProcess(11,245,284,0)
1736     If M_20# = MNext% Then M_20# = MClear%
1737     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1738         M_Out(12256)=0                  '製品チャック閉OFF
1739         M_Out(12257)=1                  '製品チャック開ON
1740         Dly 2.0
1741         Mvs PProductOnJigGet_1
1742         Mov PProductOnJigGet_2
1743         Mov PProductOnJigGet_3
1744         Mov PProductOnPltSet_3
1745         Mov PProductOnPltSet_2
1746         Mov PInitialPosition
1747         Break
1748     EndIf
1749     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1750     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1751     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1752     *CompJigGet4
1753     '
1754 '    Wait M_In(11264)=1              '製品検出センサーON
1755     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '製品検出センサーON
1756     If MRtn = 1 Then GoTo *CompJigGet5
1757     fErrorProcess(11,252,284,0)
1758     If M_20# = MNext% Then M_20# = MClear%
1759     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1760         M_Out(12256)=0                  '製品チャック閉OFF
1761         M_Out(12257)=1                  '製品チャック開ON
1762         Dly 2.0
1763         Mvs PProductOnJigGet_1
1764         Mov PProductOnJigGet_2
1765         Mov PProductOnJigGet_3
1766         Mov PProductOnPltSet_3
1767         Mov PProductOnPltSet_2
1768         Mov PInitialPosition
1769         Break
1770     EndIf
1771     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1772     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1773     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1774     *CompJigGet5
1775     '
1776     Dly 0.1
1777     Accel 50 , 100
1778     Mvs PProductOnJigGet_1          '治具上空
1779     Accel 100 , 100
1780     Ovrd 100
1781     Mov PProductOnJigGet_2          '治具上空回避点
1782     Mov PProductOnJigGet_3          'パレット-治具中間点
1783     '
1784     '製品をパレットに置く
1785     '
1786     Ovrd 60
1787     Mov PProductOnPltSet_3          '通過点
1788     Mov PProductOnPltSet_2          'パレット回避点
1789     Mov PProductOnPltSet_1          'パレット上空
1790     Ovrd 10
1791     Mvs PProductOnPltSet            'パレット置き位置
1792     Dly 0.2
1793     '
1794     *RE_PLT_SET_1
1795     '
1796     M_Out(12256)=0                  '製品チャック閉OFF
1797     M_Out(12257)=1                  '製品チャック開ON
1798     '
1799 '    Wait M_In(11265)=1              '製品チャック開センサーON
1800     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '製品チャック開センサーON
1801     If MRtn = 1 Then GoTo *CompPltSet1
1802     fErrorProcess(11,244,284,0)
1803     If M_20# = MNext% Then M_20# = MClear%
1804     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1805         Mvs PProductOnPltSet_1          'パレット上空
1806         Mov PProductOnPltSet_2          'パレット回避点
1807         Mov PInitialPosition
1808         Break
1809     EndIf
1810     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1811     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1812     If M_20# = MContinue% Then GoTo *RE_PLT_SET_1
1813     *CompPltSet1
1814     '
1815     Ovrd 100
1816     Mvs PProductOnPltSet_1          'パレット上空
1817     Mov PProductOnPltSet_2          'パレット回避点
1818     '
1819     *RE_PLT_SET_2
1820     '
1821 '    Wait M_In(11264) = 0            '製品検出センサーOFF
1822     MRtn = frInCheck(11264,0,MSETTIMEOUT05&)   '製品検出センサーOFF
1823     If MRtn = 1 Then GoTo *CompPltSet2
1824     fErrorProcess(11,253,284,0)
1825     If M_20# = MNext% Then M_20# = MClear%
1826     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1827         Mov PInitialPosition
1828         Break
1829     EndIf
1830     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1831     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1832     If M_20# = MContinue% Then GoTo *RE_PLT_SET_2
1833     *CompPltSet2
1834     '
1835 '    Mov PInitialPosition            'プログラム原点
1836     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
1837     Mov PTicketRead_1
1838     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
1839     M_20# = MAssyOK%          '正常終了
1840 '
1841 *ASSY_ERROR_END
1842     M_Out(12268) = 0            '位置決め出OFF
1843     M_Out(12269) = 1            '位置決め戻ON
1844 *AssyEnd
1845 *fnAssyStart_FEndPosi
1846     Exit Function
1847 FEnd
1848 '
1849 '■fnPiasCheck
1850 ''' <summary>
1851 ''' PIASチケット読込み
1852 ''' </summary>
1853 ''' <returns>   0 : NG
1854 '''             1 : OK(読込み完了)
1855 ''' </returns>
1856 ''' <remarks>
1857 ''' Date   : 2021/07/07 : M.Hayakawa
1858 ''' </remarks>'
1859 Function M% fnPiasCheck
1860     fnPiasCheck = 0
1861     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1862     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1863 '
1864 *RETRY_PIAS
1865     M_20# = MClear%
1866     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1867     '
1868     '【IDチケット読み込み】
1869     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1870     MInspGroup%(1) = 1              '検査G番号
1871     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1872 '
1873     'エラーの場合
1874     If MRtn <> 1 Then
1875         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1876         If MRtn <> 1 Then
1877             'D720 -> D1300 コピー要求
1878             M_Out(12565) = 1
1879             Dly 0.5
1880             M_Out(12565) = 0
1881             'エラー処理記述
1882             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1883             'GOT KEY入力待ち
1884             MKeyNumber = fnKEY_WAIT()
1885             '
1886             Select MKeyNumber
1887                 Case MNext%         '次へを選択した場合
1888                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1889                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1890                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1891                     Break
1892                 Case MAbout%        '停止を選択した場合
1893                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1894                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1895                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1896                     Break
1897                 Case MNgProcess%    'NGを選択した場合
1898                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1899                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1900                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1901                     Break
1902                 Case MContinue%     '継続を選択した場合
1903                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1904                     M_20# = MContinue%
1905                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1906                     Break
1907             End Select
1908         EndIf
1909     EndIf
1910 '----------D720 -> D1300 コピー要求----------
1911     M_Out(12565) = 1
1912     Dly 0.5
1913     M_Out(12565) = 0
1914 '----------通信確認をする----------
1915     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1916     MRtn = 0                ' 初期化
1917     M_20# = MClear%         ' 初期化
1918     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1919     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1920     If MRtn <> 1 Then
1921         If M_20# = MContinue% Then
1922             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1923         Else
1924             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1925         EndIf
1926     EndIf
1927 '----------工程抜け確認----------
1928     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1929     MRtn = 0                ' 初期化
1930     M_20# = MClear%         ' 初期化
1931     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1932     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1933     If MRtn <> 1 Then
1934         If M_20# = MContinue% Then
1935             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1936         Else
1937             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1938         EndIf
1939     EndIf
1940     '
1941     fnPiasCheck = 1
1942     *fnPiasCheck_End
1943     Exit Function
1944 FEnd
1945 '
1946 '■fnPCComuCheck
1947 ''' <summary>
1948 ''' PC-PLC通信チェック
1949 ''' </summary>
1950 ''' <returns>   0 : NG
1951 '''             1 : OK(読込み完了)
1952 ''' </returns>
1953 ''' <remarks>
1954 ''' Date   : 2021/07/07 : M.Hayakawa
1955 ''' </remarks>'
1956 Function M% fnPCComuCheck
1957     fnPCComuCheck = 0
1958     MJudge% = 0                                  '初期化
1959     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1960     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1961     '
1962     For MStaNo = 0 To 5
1963         '
1964         If M_In(MIN_PIAS_ComOK%) = 1 Then
1965             'PC通信OK(M400)
1966             MJudge% = MOK%
1967             MStaNo = 5
1968             Break
1969         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1970             'toRBT_通信確認time out
1971             MJudge% = MNG%
1972             MCommentD1001 = 15
1973             MCommentD1002 = 21
1974             MStaNo = 5
1975             Break
1976         Else
1977             'toRBT_通信確認time out
1978             MJudge% = MNG%
1979             MCommentD1001 = 14
1980             MCommentD1002 = 21
1981             Break
1982         EndIf
1983     Next MStaNo
1984     '
1985     '上記で返信フラグを受信してからPC通信確認OFF
1986     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1987     '
1988     'エラー画面
1989     If MJudge% <> MOK% Then
1990         M_20# = MClear%     '初期化
1991         'エラー処理記述
1992         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1993         'GOT KEY入力待ち
1994         MKeyNumber = fnKEY_WAIT()
1995         '
1996         If MKeyNumber = MAbout% Then            '停止を選択した場合
1997             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1998             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1999             Break
2000         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
2001             M_20# = MNext%                      'M_20# プログラム間共通外部変数
2002             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2003             Break
2004         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
2005             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
2006             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2007             Break
2008         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
2009             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
2010             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2011             Break
2012         EndIf
2013     Else
2014         'OKの場合
2015         fnPCComuCheck = 1
2016     EndIf
2017     Exit Function
2018 FEnd
2019 '
2020 '■fnProcessCheck
2021 ''' <summary>
2022 ''' 工程抜け確認
2023 ''' </summary>
2024 ''' <returns>    1：工程履歴OK     0：異常終了
2025 '''             -1：前工程履歴NG  -2：自工程履歴あり
2026 '''             -3：モデル仕向NG  -4：タイムアウト
2027 '''             -5：履歴処理エラー
2028 ''' </returns>
2029 ''' <remarks>
2030 ''' Date   : 2021/07/07 : M.Hayakawa
2031 ''' </remarks>'
2032 Function M% fnProcessCheck
2033     fnProcessCheck = 0
2034     MJudge% = MNG%      '一旦NGを初期化とする
2035 '----------工程抜け確認----------
2036     MCommentD1001 = 0   'コメント初期化
2037     For MStaNo = 0 To 5
2038         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
2039         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
2040         '
2041         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
2042             MJudge% = MOK%
2043             fnAutoScreenComment(85)     ' AUTO画面
2044             MStaNo = 5
2045             Break
2046         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
2047             MFlgLoop% = 0
2048             MJudge% = MNG%
2049             MCommentD1001 = 27
2050             MCommentD1002 = 22
2051             fnAutoScreenComment(94)     ' AUTO画面
2052             fnProcessCheck = -2         ' NGは-2を返す
2053             MStaNo = 5
2054             Break
2055         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
2056            MJudge% = MNG%
2057             MCommentD1001 = 31
2058             MCommentD1002 = 22
2059             fnAutoScreenComment(83)     ' AUTO画面
2060             fnProcessCheck = -3         ' NGは-3を返す
2061             MStaNo = 5
2062             Break
2063         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
2064             '履歴NGは直ぐに終了せず繰り返し確認を行う
2065             '前工程の書込みが終了していない可能性があるため
2066             MJudge% = MNG%
2067             MCommentD1001 = 32
2068             MCommentD1002 = 22
2069             fnAutoScreenComment(84)     ' AUTO画面
2070             fnProcessCheck = -1         ' NGは-1を返す
2071             Dly 1.0
2072             '工程抜け確認OFF
2073             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
2074             Dly 1.0
2075            'MStaNo = 5
2076             Break
2077         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
2078             MFlgLoop% = 0
2079             MJudge% = MNG%
2080             MCommentD1001 = 29
2081             MCommentD1002 = 22
2082             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
2083             fnProcessCheck = -5         ' NGは-5を返す
2084             MStaNo = 5
2085             Break
2086         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
2087             MJudge% = MNG%
2088             If MCommentD1001 = 32 Then
2089                 '何もしない
2090             Else
2091                 MCommentD1001 = 26
2092             EndIf
2093             MCommentD1002 = 22
2094             fnProcessCheck = -4         ' NGは-4を返す
2095             MStaNo = 5
2096             Break
2097         Else
2098             MJudge% = MNG%
2099             MCommentD1001 = 28
2100             MCommentD1002 = 22
2101         EndIf
2102     Next MStaNo
2103     '工程抜け確認OFF
2104     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
2105     '通過履歴NG 工程抜けの場合
2106     If MJudge% = MPass% Then
2107         M_20# = MPass%
2108     EndIf
2109     '
2110     'エラー画面
2111     If MJudge% <> MOK% Then
2112         M_20# = MClear%     '初期化
2113         'エラー処理記述
2114         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2115         'GOT KEY入力待ち
2116         MKeyNumber = fnKEY_WAIT()
2117         '
2118         Select MKeyNumber
2119             Case MAbout%        '停止を選択した場合
2120                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
2121                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2122                 Break
2123             Case MNext%         '次へを選択した場合
2124                 M_20# = MPass%          'M_20# プログラム間共通外部変数
2125                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2126                 Break
2127             Case MContinue%     '継続を選択した場合
2128                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
2129                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2130                 Break
2131             Case MNgProcess%    'NGを選択した場合
2132                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
2133                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2134                 Break
2135         End Select
2136     Else
2137         fnProcessCheck = 1  ' OKは1を返す
2138     EndIf
2139     Exit Function
2140 FEnd
2141 '
2142 '■fnPiasWrite
2143 ''' <summary>
2144 ''' Pias 組立結果書込み要求
2145 ''' </summary>
2146 '''<param name="MFlg%">
2147 '''                 MOK%(1) = 工程履歴にOKを書込む
2148 '''                 MNG%(0) = 工程履歴にNGを書込む
2149 '''</param>
2150 '''<returns></returns>
2151 ''' <remarks>
2152 ''' Date   : 2021/07/07 : M.Hayakawa
2153 ''' </remarks>'
2154 Function M% fnPiasWrite(ByVal MFlg%)
2155       fnPiasWrite = 0
2156 *RETRY_PIASWRITE
2157     '
2158     '組立OK(MOK%)の場合　M306 ON
2159    '組立NG(MNG%)の場合　M307 ON
2160     If MFlg% = MOK% Then
2161         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
2162     Else
2163         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
2164     EndIf
2165     Dly 0.1                  '念のため
2166     '
2167     'Piasへ書込み開始 M305 -> ON
2168     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
2169     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
2170     '
2171     MJudge% = MNG%
2172     '
2173     For MStaNo = 0 To 5
2174         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
2175             MJudge% = MOK%
2176             'MRet = fnAutoScreenComment(85)  'AUTO画面
2177             MStaNo = 5
2178             Break
2179         '
2180         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
2181             MJudge% = MNG%
2182             'MRet = fnAutoScreenComment(85)  'AUTO画面
2183            MCommentD1001 = 34
2184            MCommentD1002 = 25
2185             MStaNo = 5
2186             Break
2187         '
2188         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
2189             MJudge% = MNG%
2190             'MRet = fnAutoScreenComment(85)  'AUTO画面
2191            MCommentD1001 = 35
2192            MCommentD1002 = 25
2193             MStaNo = 5
2194             Break
2195         '
2196         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
2197             MJudge% = MNG%
2198             'MRet = fnAutoScreenComment(85)  'AUTO画面
2199            MCommentD1001 = 36
2200            MCommentD1002 = 25
2201             MStaNo = 5
2202             Break
2203         '
2204         Else
2205             MJudge% = MNG%
2206            MCommentD1001 = 42
2207            MCommentD1002 = 25
2208         '
2209         EndIf
2210         '
2211     Next MStaNo
2212     '
2213     'Piasへ書込み開始 M305 -> OfF
2214     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
2215     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
2216     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
2217     '
2218     '
2219     '通過履歴NG 工程抜けの場合
2220     If MJudge% = MPass% Then
2221         M_20# = MPass%
2222     EndIf
2223     '
2224    M_20# = MClear%     '初期化
2225     '
2226     'エラー画面
2227     If MJudge% < MOK% Then
2228     '
2229 '残しておくが現状では使用しないラベル
2230 *RETRY_ERR_WRITE
2231         M_20# = MClear%     '初期化
2232         'エラー処理記述
2233         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2234         'GOT KEY入力待ち
2235         MKeyNumber = fnKEY_WAIT()
2236         '
2237         If MKeyNumber = MAbout% Then   '停止を選択した場合
2238             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2239            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2240             Break
2241         '
2242         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2243             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2244             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2245         '
2246         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2247             M_20# = MPass%            'M_20# プログラム間共通外部変数
2248             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2249         '
2250         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2251             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2252            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2253             Break
2254         '
2255         EndIf
2256         '
2257         If M_20# = MClear% Then *RETRY_ERR_WRITE
2258         '
2259     EndIf
2260     '
2261     If M_20# = MContinue% Then *RETRY_PIASWRITE
2262     '
2263     fnPiasWrite = 1
2264     Exit Function
2265 FEnd
2266 '
2267 '■fnPCBNumberCheck
2268 ''' <summary>
2269 ''' Pias 基板番号照合要求
2270 ''' </summary>
2271 '''<param name="%"></param>
2272 '''<param name="%"></param>
2273 '''<returns></returns>
2274 ''' <remarks>
2275 ''' Date   : 2021/07/07 : M.Hayakawa
2276 ''' </remarks>'
2277 Function M% fnPCBNumberCheck
2278       fnPCBNumberCheck = 0
2279     '
2280 *RETRY_PCBCHECK
2281     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
2282     'Piasへ基板照合開始 M310 -> ON
2283     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
2284     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
2285     '
2286     MJudge% = MNG%
2287     '
2288     For MStaNo = 0 To 5
2289         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
2290             MJudge% = MOK%
2291             fnAutoScreenComment(96)  'AUTO画面
2292             MStaNo = 5
2293             Break
2294         '
2295         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
2296             MJudge% = MNG%
2297             fnAutoScreenComment(97)  'AUTO画面
2298             MCommentD1001 = 37
2299             MCommentD1002 = 25
2300             MStaNo = 5
2301             Break
2302         '
2303         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
2304             MJudge% = MNG%
2305             fnAutoScreenComment(98)  'AUTO画面
2306             MCommentD1001 = 38
2307             MCommentD1002 = 25
2308             MStaNo = 5
2309             Break
2310         '
2311         ElseIf M_In(11580) = 1 Then                         'time out
2312             MJudge% = MNG%
2313             fnAutoScreenComment(99)  'AUTO画面
2314             MCommentD1001 = 39
2315             MCommentD1002 = 25
2316             MStaNo = 5
2317             Break
2318         '
2319         Else
2320             MJudge% = MNG%
2321            MCommentD1001 = 41
2322            MCommentD1002 = 25
2323         '
2324         EndIf
2325         '
2326     Next MStaNo
2327     '
2328     'Piasへ基板照合開始 M310 -> OfF
2329     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
2330     '
2331     '
2332     '通過履歴NG 工程抜けの場合
2333     If MJudge% = MPass% Then
2334         M_20# = MPass%
2335     EndIf
2336     '
2337    M_20# = MClear%     '初期化
2338     '
2339     'エラー画面
2340     If MJudge% < MOK% Then
2341     '
2342 '残しておくが現状では使用しないラベル
2343 *RETRY_ERR_PCBNUMBER
2344         M_20# = MClear%     '初期化
2345         'エラー処理記述
2346         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2347         'GOT KEY入力待ち
2348         MKeyNumber = fnKEY_WAIT()
2349         '
2350         If MKeyNumber = MAbout% Then   '停止を選択した場合
2351             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2352             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2353             Break
2354         '
2355         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2356             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2357             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2358         '
2359         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2360             M_20# = MPass%            'M_20# プログラム間共通外部変数
2361             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2362         '
2363         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2364             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2365             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2366             Break
2367         '
2368         EndIf
2369         '
2370         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
2371         '
2372     EndIf
2373     '
2374     If M_20# = MContinue% Then *RETRY_PCBCHECK
2375     Exit Function
2376 FEnd
2377 '
2378 '■ScrewTight
2379 ''' <summary>
2380 ''' ねじ締めを行う(Sタイト)
2381 ''' </summary>
2382 '''<param name="PScrewPos()">
2383 '''             PScrewPos(1)    ：パレット上ねじ締めS?@の安全回避位置  +30
2384 '''             PScrewPos(2)    ：ねじ締め回避点
2385 '''             PScrewPos(10)   ：ねじ締め終了高さ
2386 '''<param name="MScrewType">ネジタイプ(mm/sec)
2387 '''             1:6mm Sタイト銀ネジ
2388 '''             2:13mm Sタイト銀ネジ
2389 '''             3:6mm Sタイト黒ネジ
2390 '''             4:3mm Sタイト黒ネジ
2391 '''             5:6mm Mネジ
2392 '''             6:13mm Sタイト銀ネジ(ねじ締めパラメータ違い)
2393 '''</param>
2394 '''<param name="MFeedSpd">送り速度(mm/sec)</param>
2395 '''<returns>整数
2396 '''         0=異常終了、1=正常終了
2397 '''</returns>
2398 ''' <remarks>
2399 ''' Date   : 2021/07/07 : M.Hayakawa
2400 ''' Update : 2021/09/28 : M.Hayakawa ネジタイプ、送り速度を引数に追加
2401 ''' </remarks>'
2402 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   'ネジ締め個別設定
2403     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2404     ScrewTight = 0
2405     MOKNGFlg = 0
2406     Ovrd 100
2407     Fine 0.05 , P
2408     Mvs PScrewPosition(1)       ' パレット上ねじ締めS?@の安全回避位置
2409     Select MScrewType%      '読み込み位置変更(1/19中村)
2410         Case 1
2411             ' Sタイト：プログラム1、バンク1に設定
2412             ProgramBankSet(1,1)
2413             Break
2414         Case 2
2415             ' Sタイト13mm：プログラム2、バンク1に設定
2416             ProgramBankSet(2,1)
2417             Break
2418         Case 3
2419             ' Sタイト黒：プログラム3、バンク1に設定
2420             ProgramBankSet(3,1)
2421             Break
2422         Case 4
2423             ' Sタイト3mm黒：プログラム4、バンク1に設定
2424             ProgramBankSet(4,1)
2425             Break
2426         Case 5
2427             ' Mネジ：プログラム5、バンク1に設定
2428             ProgramBankSet(5,1)
2429             Break
2430         Case 6
2431             ' Sタイト13mm(パラメータ違い):プログラム2バンク2に設定
2432             ProgramBankSet(2,2)
2433             Break
2434         Default
2435             ' プログラム1、バンクなし設定
2436             ProgramBankSet(0,0)
2437             Break
2438     End Select
2439     Accel 100,10
2440 '    Ovrd MOvrdA%               '10/7現在値Null
2441     Ovrd 60                     '念のため減速 数値変更 林
2442     ' パレット上ねじ締め開始位置へ移動
2443     Mvs PScrewPosition(2)
2444     ' 内部Ovrd設定
2445 '    Ovrd MOvrdA%
2446     Ovrd 100
2447     Accel
2448     ' Spd設定
2449     Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
2450 '    Spd MFeedSpd
2451     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
2452     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
2453 '    Select MScrewType%      '読み込み位置変更(1/19中村)
2454 '        Case 1
2455 '            ' Sタイト：プログラム1、バンク1に設定
2456 '            ProgramBankSet(1,1)
2457 '            Break
2458 '        Case 2
2459 '            ' Sタイト13mm：プログラム2、バンク1に設定
2460 '            ProgramBankSet(2,1)
2461 '            Break
2462 '        Case 3
2463 '            ' Sタイト黒：プログラム3、バンク1に設定
2464 '            ProgramBankSet(3,1)
2465 '            Break
2466 '        Case 4
2467 '            ' Sタイト3mm黒：プログラム4、バンク1に設定
2468 '            ProgramBankSet(4,1)
2469 '            Break
2470 '        Case 5
2471 '            ' Mネジ：プログラム5、バンク1に設定
2472 '            ProgramBankSet(5,1)
2473 '            Break
2474 '        Default
2475 '            ' プログラム1、バンクなし設定
2476 '            ProgramBankSet(0,0)
2477 '            Break
2478 '    End Select
2479 '
2480 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
2481      'ドライバーON　CW
2482     M_Out(12241)=1
2483     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
2484     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
2485     Dly 0.1
2486     Spd M_NSpd
2487     Fine 0 , P
2488     '
2489     If M_In(11256)=1 Then  'ねじトータルエラー検出時
2490         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2491         Dly 0.1
2492        ' プログラム・バンク解除
2493         ProgramBankSet(0,0)
2494         'パレット上ねじ締め終了位置上空へ移動
2495         Mvs PScrewPosition(10),-80
2496         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
2497         M_Out(12249)=1 Dly 0.3
2498         MOKNGFlg = -1
2499         ScrewTight = 0
2500     Else
2501          'ドライバーOFF　CW
2502         M_Out(12241)=0
2503 '        エラーがない場合はネジ締め終了位置で増し締め
2504         Select MScrewType%
2505             Case 1
2506                 ' Sタイト：プログラム1、バンク3に設定
2507                 ProgramBankSet(1,3)
2508                 Break
2509             Case 2
2510                 ' Sタイト13mm：プログラム2、バンク3に設定
2511                 ProgramBankSet(2,3)
2512                 Break
2513             Case 3
2514                 ' Sタイト黒：プログラム1、バンク3に設定
2515                 ProgramBankSet(3,3)
2516                 Break
2517             Case 4
2518                 ' Sタイト13mm：プログラム1、バンク3に設定
2519                 ProgramBankSet(4,3)
2520                 Break
2521             Case 5
2522                 ' Mネジ：プログラム1、バンク3に設定
2523                 ProgramBankSet(5,3)
2524                 Break
2525             Default
2526                 ' プログラム1、バンクなし設定
2527                 ProgramBankSet(0,0)
2528                 Break
2529         End Select
2530          'ドライバーON　CW
2531         Mvs PScrewPosition(10)
2532         M_Out(12241)=1
2533         Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
2534 '
2535          'ドライバーOFF　CW
2536         M_Out(12241)=0
2537        ' プログラム・バンク解除
2538         ProgramBankSet(0,0)
2539         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
2540         M_Out(12249)=1 Dly 0.3
2541     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
2542         'パレット上ねじ締め終了位置上空へ移動
2543         Mvs PScrewPosition(10),-80
2544         ScrewTight = 1
2545     EndIf
2546 ' 暫定（暫定マスク　9/16 M.Hayakawa)
2547 '    Ovrd 10
2548 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS?@の安全回避位置
2549     Ovrd 100
2550     Exit Function
2551 FEnd
2552 '
2553 '■ScrewGet
2554 ''' <summary>
2555 ''' ねじ供給機からねじを得る
2556 ''' </summary>
2557 '''<param name="%">
2558 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
2559 '''         PScrewPos(2)    ：ねじ供給器回避点
2560 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
2561 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2562 '''         PScrewPos(3)    ：Mねじポカヨケ位置
2563 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
2564 '''</param>
2565 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
2566 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
2567 '''<returns>整数
2568 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
2569 '''</returns>
2570 ''' <remarks>
2571 ''' Date   : 2021/07/07 : M.Hayakawa
2572 ''' </remarks>
2573 '''<update>
2574 '''Date    : 2021/11/15 : 中村
2575 '''</update>
2576 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
2577     fnAutoScreenComment(522)    '状態表示[ネジ供給待ち] 2022/05/09 渡辺
2578     ScrewGet = 0
2579     MScrewJudge% = 0
2580     MFinCnt% = 2
2581     'ねじ供給器初期動作エラーチェック
2582 ' ↓暫定削除
2583     Mov PScrewPosition(2)   'ねじ供給機回避点へ移動
2584     For MCnt% = 0 To MFinCnt%
2585        'ねじ供給器初期動作エラーチェック
2586         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
2587         If MRtn = 0 Then
2588             'Ovrd 30
2589             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
2590             ScrewGet = -1
2591             MScrewJudge% = 2
2592         EndIf
2593         Ovrd 100
2594         If FeederScrewSensor% <> 0 Then
2595             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
2596                 'Ovrd 30
2597                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
2598                 'NGとしてここの関数から抜ける
2599                 ScrewGet = -2
2600                 MScrewJudge% = 3
2601             EndIf
2602         EndIf
2603         Ovrd 100
2604         Spd M_NSpd
2605         'ねじ供給開始
2606         If MScrewJudge% = 0 Then
2607     '        ScrewGet = 0
2608             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2609             MScrewCnt% = 0
2610             MFinCnt% = 2
2611             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2612             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2613             'Ovrd 5 '2に変更 10/6 M.H '5に変更10/7中村
2614             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2615             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2616             Mvs PScrewPosition(10), 1.2
2617             M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
2618             'ビット回転
2619             M_Out(Y60_Driver)=1
2620             Dly 0.2
2621             '
2622             'Ovrd 5 '2に変更 10/6 M.H '5に変更10/7中村　削除林
2623             JOvrd M_NJovrd
2624             Spd M_NSpd
2625             'ネジ吸着確認位置移動
2626             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2627             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
2628             'ビット回転停止
2629             M_Out(Y60_Driver)=0
2630             '
2631             '1秒間ネジ吸着確認
2632             MRtn = frInCheck(11268, 1, MSETTIMEOUT01&)
2633             'MRtn = 0'強制エラー
2634             '吸着エラーの場合
2635             'ネジをねじ太郎に戻す
2636             If MRtn = 0 Then
2637                 Ovrd 5      '2から5に変更
2638                 'ビット回転停止
2639                 M_Out(Y60_Driver)=0
2640                 'ネジ供給機上空
2641                 Mvs PScrewPosition(1)
2642                 '更に上空
2643                 Mov PScrewPosition(1), -140
2644                 'ネジ捨て位置
2645                 If FeederReadyNo% = 11259 Then     '供給機別に吸着エラー数をカウント　2022/05/19 渡辺
2646                     MRtn = FnCtlValue2(3)          '供給機２吸着エラー数＋１
2647                 Else
2648                     MRtn = FnCtlValue2(4)          '供給機１吸着エラー数＋１  2022/04/28 渡辺
2649                 EndIf
2650                 Mov PScrewPosition(9)
2651                 MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
2652                 '吸着OFF
2653                 M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
2654                 Dly 0.2
2655                 '破壊ON
2656                 M_Out(Y6B_VB1)=1 '真空破壊ON
2657                 'ビット回転
2658                 M_Out(Y61_Driver)=1
2659                 Dly 0.5
2660                 '                '
2661                 Ovrd 100
2662                 JOvrd M_NJovrd
2663                 Spd M_NSpd
2664                 'ドライバーを上下させねじを振り落とす
2665                 Mov PScrewPosition(9), 10
2666                 Mov PScrewPosition(9)
2667                 Dly 0.1
2668                 Mov PScrewPosition(9), 10
2669                 Mov PScrewPosition(9)
2670                 '
2671                 'ネジ落ち待ち
2672                 Wait M_In(11268) = 0
2673                 'ビット回転停止
2674                 M_Out(Y61_Driver)=0
2675                 Dly 0.1
2676                 '破壊OFF
2677                 M_Out(Y6B_VB1)=0 '真空破壊OFF
2678                 'ねじ落ちたとして、移動更に上空
2679                 Mov PScrewPosition(1), -140
2680                 Ovrd 100
2681                 Spd M_NSpd
2682                 'ネジ供給機上空
2683                 Mvs PScrewPosition(1)
2684 '                '
2685                 ScrewGet = -3
2686                 Break
2687 '                '
2688             Else
2689                 MCnt% = MFinCnt%
2690                 ScrewGet = 0
2691             EndIf
2692         Else
2693             MCnt% =MFinCnt%
2694         EndIf
2695     Next  MCnt%
2696         '
2697     If MScrewJudge% = 0 Then
2698         Ovrd 100
2699         Spd M_NSpd
2700         Mvs PScrewPosition(10), -20  ' ねじピックアップ位置 -20mm
2701         M_Out(Y60_Driver)=0     ' ビット回転停止
2702         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2703         Mvs PScrewPosition(10), -20  ' ねじピックアップ位置 -20mm
2704         Mov PScrewPosition(2)
2705         'もう一度吸着確認
2706         MRtn = frInCheck(11268, 1, MSETTIMEOUT01&)
2707         If MRtn = 0 Then      '吸着エラーの場合
2708             MScrewJudge% = 4
2709             ScrewGet = -3
2710         ElseIf MRtn = 1 Then      '吸着OKの場合
2711             MScrewJudge% = 1
2712             ScrewGet = 1
2713         EndIf
2714         Break
2715     EndIf
2716     If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
2717     '
2718     Select MScrewJudge%
2719         Case 0
2720 '            fErrorProcess(11,162,163,0) '異常終了
2721             MCommentD1001 = 162
2722             MCommentD1002 = 96
2723             Break
2724         Case 2
2725 '            fErrorProcess(11,63,161,0) '供給NG
2726             MCommentD1001 = 63
2727             MCommentD1002 = 96
2728             Break
2729         Case 3
2730 '            fErrorProcess(11,160,164,0) '誤供給
2731             MCommentD1001 = 237
2732             MCommentD1002 = 96
2733             Break
2734         Case 4
2735 '            fErrorProcess(11,94,95,0) '吸着NG
2736             MCommentD1001 = 94
2737             MCommentD1002 = 95
2738             Break
2739     End Select
2740     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
2741     '
2742     Select M_20#
2743         Case MAbout%          '停止が押された場合
2744             Mov PScrewPosition(2)                  '初期位置に戻って停止処理
2745 '            Mov PInitialPosition
2746             Break
2747         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
2748             Break
2749         Case MNext%           '継続が押された場合
2750             M_20# = MClear%     '初期化
2751             Break
2752         Case MNgProcess%      'NGが押された場合
2753             Mov PScrewPosition(2)   'PIASにNG書き込みを行い,初期位置に戻って行程終了
2754 '            Mov PInitialPosition
2755             Break
2756         End Select
2757 *End_ScrewGet
2758     Exit Function
2759 FEnd
2760 '
2761 '■ProgramBankSet
2762 ''' <summary>
2763 ''' ねじ締めを行う(Pタイト)
2764 ''' </summary>
2765 '''<param name="MProgramNo">プログラム番号</param>
2766 '''<param name="MBankNo">バンク番号</param>
2767 '''</returns>
2768 ''' <remarks>
2769 ''' Date   : 2021/10/05 : M.Hayakawa
2770 ''' </remarks>'
2771 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
2772 '
2773     MLocalPrgNo% = (MProgramNo% - 1) * 32
2774     MLocalBankNo% = MBankNo% * 4
2775 '
2776     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
2777         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
2778     Else
2779         MLocalOutNo% = 0
2780     EndIf
2781 '
2782     M_Out8(12240) = MLocalOutNo%
2783     Dly 0.1
2784     Exit Function
2785 FEnd
2786 '
2787 '■fnKEY_WAIT()
2788 ''' <summary>
2789 ''' GOTからのキー入力待ち
2790 ''' </summary>
2791 '''<returns>1：停止    2：次へ
2792 '''         3：継続    4：トルクチェック開始
2793 '''         5：NG
2794 '''         11：ロボット初期位置1    12：ロボット初期位置2
2795 '''         13：ロボット初期位置3    14：ロボット初期位置4
2796 '''</returns>
2797 ''' <remarks>
2798 ''' Date   : 2021/07/07 : M.Hayakawa
2799 ''' </remarks>'
2800 Function M% fnKEY_WAIT()
2801     fnKEY_WAIT = 0
2802     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2803     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2804     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2805     '下記キー待ちの継続に反応させないため
2806     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2807     Dly 0.2
2808     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2809     MLocalLoopFlg=1
2810     While MLocalLoopFlg=1
2811         If M_In(11345) = 1 Then         '停止   M5345
2812             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2813             fnKEY_WAIT = 1
2814             MLocalLoopFlg=-1
2815             Break
2816         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2817             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2818             fnKEY_WAIT = 2
2819             MLocalLoopFlg=-1
2820             Break
2821         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2822             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2823             fnKEY_WAIT = 3
2824             MLocalLoopFlg=-1
2825             Break
2826         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2827             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2828             fnKEY_WAIT = 4
2829             MLocalLoopFlg=-1
2830             Break
2831         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2832             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2833             fnKEY_WAIT = 5
2834             MLocalLoopFlg=-1
2835             Break
2836             '
2837         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2838             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2839             fnKEY_WAIT = MRobotInit1%
2840             MLocalLoopFlg=-1
2841             Break
2842             '
2843         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2844             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2845             fnKEY_WAIT = MRobotInit2%
2846             MLocalLoopFlg=-1
2847             Break
2848             '
2849         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2850             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2851             fnKEY_WAIT = MRobotInit3%
2852             MLocalLoopFlg=-1
2853             Break
2854             '
2855         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2856             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2857             fnKEY_WAIT = MRobotInit4%
2858             MLocalLoopFlg=-1
2859             Break
2860             '
2861         Else
2862         EndIf
2863     WEnd
2864     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2865     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2866     Exit Function
2867 FEnd
2868 '
2869 '■ fnAUTO_CTL
2870 ''' <summary>
2871 ''' AUTOモードOFF、PLCからの開始待ち
2872 ''' </summary>
2873 ''' <remarks>
2874 ''' Date   : 2021/07/07 : M.Hayakawa
2875 ''' </remarks>
2876 Function M% fnAUTO_CTL
2877     fnAUTO_CTL = 0
2878     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2879     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2880     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2881     '
2882     If M_Svo=0 Then             'サーボON確認
2883         Servo On
2884     EndIf
2885     Wait M_Svo=1
2886     Exit Function
2887 FEnd
2888 '
2889 '■ fnWindScreenOpen
2890 ''' <summary>
2891 ''' ウィンド画面の表示、非表示設定
2892 ''' </summary>
2893 '''<param name="%"></param>
2894 '''<param name="%"></param>
2895 '''<param name="%"></param>
2896 '''<param name="%"></param>
2897 ''' <remarks>
2898 ''' コメントD1001, D1002, D1003の設定
2899 ''' MWindReSet = 0     画面非表示
2900 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2901 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2902 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2903 ''' Date   : 2021/07/07 : M.Hayakawa
2904 ''' </remarks>
2905 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2906     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2907         M_Out16(12480) = MCommentD1001            'D1001 コメント
2908     EndIf
2909     '
2910     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2911         M_Out16(12496) = MCommentD1002            'D1002 コメント
2912     EndIf
2913     '
2914     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2915        M_Out16(12512) = MCommentD1003            'D1003 コメント
2916     EndIf
2917     '
2918     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2919     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2920     Dly 0.5
2921     M_Out(12363) = 0                         'ウィンド画面設定
2922     Exit Function
2923 FEnd
2924 '
2925 '■FnCtlValue2
2926 ''' <summary>
2927 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2928 ''' </summary>
2929 ''' <param name="MCtlNo%"></param>
2930 ''' <remarks>
2931 ''' Date : 2022/04/28 渡辺
2932 ''' </remarks>
2933 '''
2934 '''  1：投入数       ＋１
2935 '''  2：組立ＯＫ数   ＋１
2936 '''  3：供給機２吸着エラー数 ＋１　　組立NGから変更 2022/05/19 渡辺
2937 '''  4：供給機１吸着エラー数 ＋１
2938 ''' 99：読書開始信号 OFF
2939 '''
2940 Function M% FnCtlValue2(ByVal MCtlNo%)
2941     FnCtlValue2 = 1
2942     Select MCtlNo%
2943         Case 1        '投入数＋１
2944             M_Out(12569) = 0             '書込み開始信号OFF
2945             M_Out(12568) = 1             '読込み開始信号ON
2946             MInputQty = M_In16(11600)    '投入数受信
2947             MInputQty = MInputQty + 1    '投入数＋１
2948             M_Out16(12592) = MInputQty   '投入数送信
2949             M_Out(12569) = 1             '書込み開始信号ON
2950             Break
2951             '
2952         Case 2        '組立ＯＫ数＋１
2953             M_Out(12569) = 0             '書込み開始信号OFF
2954             M_Out(12568) = 1             '読込み開始信号ON
2955             MAssyOkQty = M_In16(11616)   '組立OK数受信
2956             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2957             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2958             M_Out(12569) = 1             '書込み開始信号ON
2959             Break
2960             '
2961         Case 3        '供給機２吸着エラー数＋１
2962             M_Out(12569) = 0                       '書込み開始信号OFF
2963             M_Out(12568) = 1                       '読込み開始信号ON
2964             MSuctionErrQty = M_In16(11632)         '供給機２吸着エラー数受信
2965             MSuctionErrQty = MSuctionErrQty + 1    '供給機２吸着エラー数＋１
2966             M_Out16(12624) = MSuctionErrQty        '供給機２吸着エラー数送信
2967             M_Out(12569) = 1                       '書込み開始信号ON
2968             Break
2969             '
2970         Case 4        '供給機１吸着エラー数＋１
2971             M_Out(12569) = 0                       '書込み開始信号OFF
2972             M_Out(12568) = 1                       '読込み開始信号ON
2973             MSuctionErrQty = M_In16(11648)         '供給機１吸着エラー数受信
2974             MSuctionErrQty = MSuctionErrQty + 1    '供給機１吸着エラー数＋１
2975             M_Out16(12640) = MSuctionErrQty        '供給機１吸着エラー数送信
2976             M_Out(12569) = 1                       '書込み開始信号ON
2977             Break
2978             '
2979         Case 99        '読書開始信号OFF
2980             M_Out(12568) = 0        '読込み開始信号OFF
2981             M_Out(12569) = 0        '書込み開始信号OFF
2982             Break
2983             '
2984     End Select
2985     Exit Function
2986 FEnd
2987 '
2988 '
2989 '■FnScreEroorCord
2990 ''' 電動ドライバーのエラーコードを含めたコメントを出す為のコメント番号の作成
2991 ''' 新規作成：2022/05/23 : 渡辺
2992 '''
2993 Function M% FnScreEroorCord()
2994     MScrewErrorCord% = 0
2995     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2996     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2997     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2998     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2999     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
3000     MScrewErrorCord% = MScrewErrorCord% * 10
3001     MScrewErrorCord% = MScrewErrorCord% + 500
3002     FnScreEroorCord = MScrewErrorCord%
3003     Exit Function
3004 FEnd
3005 '
3006 '
3007 'Insightによる画像処理検査実行（並列処理なし）
3008 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
3009 '-------------------------------------------------------------------------------
3010 'Insightによる画像処理検査実行（並列処理なし）
3011 '   引数
3012 '       PInspPos()      ：検査位置
3013 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
3014 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
3015 '       MInspCnt%       ：検査位置数
3016 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
3017 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
3018 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
3019 '   戻り値：整数
3020 '       0=異常終了、1=正常終了
3021 '
3022 '   MInspErrNum     ：異常終了時にエラー番号が設定される
3023 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
3024 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
3025 '   20190820    :   引数 MZAxis%,MNgContinue 追加
3026 '   20200410    :   検査グループ設定Retry追加
3027 '-------------------------------------------------------------------------------
3028     '----- 初期設定 -----
3029     Cnt 0                                                           '移動効率化解除(初期値=0)
3030     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
3031 '    Cnt 1,0.1,0.1
3032     '変数宣言・初期化
3033     Def Inte MNum                                                   '検査番号(検査順1〜)
3034     MNum% = 1                                                       '検査番号初期値設定
3035     Def Inte MEndFlg                                                '検査終了フラグ
3036     MEndFlg% = 0
3037     '
3038     '検査G番号設定要求・検査実行要求off
3039     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
3040     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
3041     'エラー番号クリア
3042     MInspErrNum = 0                                                 '検査実行エラー番号
3043     M_Out16(MOUT_InspErrNum) = MInspErrNum
3044     MInspNGStepNum = 0                                              '検査実行NGStep番号
3045     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
3046     '
3047     'Insight Ready check?
3048     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
3049         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
3050         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
3051         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
3052         ISInspectionSingle = 0                                      '異常終了戻り値設定
3053         Exit Function
3054     EndIf
3055     '
3056     '検査位置数確認
3057     If MInspCnt% < 1 Or 30 < MInspCnt% Then
3058         MInspErrNum = 21                                            '検査データなし 21　引数<1
3059         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
3060         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
3061         ISInspectionSingle = 0                                      '異常終了戻り値設定
3062         Exit Function
3063     EndIf
3064     '
3065     '
3066     '
3067     '----- メイン処理 -----
3068     '設定された検査位置数分の検査実行
3069     While( MEndFlg% = 0 )
3070         '----- 検査グループ番号設定Retry追加 20200410
3071         MSetGrNumRetryExitFlg = 0
3072         MSetGrNumRetryCnt = 2                                           'Retry回数設定
3073         While( MSetGrNumRetryExitFlg = 0 )
3074         '----- 検査グループ番号設定Retry追加ここまで 20200410
3075             '
3076             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
3077             '
3078             '----- 検査グループ番号設定 -----
3079             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
3080             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
3081             '
3082             '検査位置へ移動・移動完了待ち
3083             fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
3084             Mvs PInspPos( MNum% )                                       '移動
3085             fnAutoScreenComment(523)                                    '状態表示[画像処理検査中] 2022/05/09 渡辺
3086             Dly 0.05                                                    '移動完了後Delay
3087             '
3088             '検査グループ番号設定終了確認
3089             M_Timer(1) = 0
3090             MExitFlg = 0
3091             While( MExitFlg = 0 )
3092                 '検査G設定正常終了?
3093                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
3094                     MExitFlg = 1
3095                 '
3096                 '検査G設定異常終了?
3097                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
3098                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
3099                     If MInspErrNum = 0 Then                             '1回目のエラー?
3100                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
3101                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
3102                     EndIf
3103                     MExitFlg = 1
3104                 '
3105                 'timeoutチェック
3106                 ElseIf 1000 < M_Timer(1) Then
3107                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
3108                     If MInspErrNum = 0 Then                             '1回目のエラー?
3109                         MInspErrNum = 12                                'timeout エラー番号=12
3110                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
3111                     EndIf
3112                     MExitFlg = 1
3113                 EndIf
3114             WEnd
3115             '
3116             '検査G番号設定要求off
3117             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
3118             '
3119             '----- 検査グループ設定Retry追加 20200410
3120             'NGなければ抜ける
3121             If MCurrentStepErr = 0 Then
3122                 MSetGrNumRetryExitFlg = 1
3123             Else
3124                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
3125                 If MSetGrNumRetryCnt = 0 Then
3126                     MSetGrNumRetryExitFlg = 1
3127                 Else
3128                     'Retryへ　その前にDelay
3129                     Dly 0.5
3130                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
3131                 EndIf
3132             EndIf
3133             '----- 検査グループ設定Retry追加ここまで 20200410
3134             '
3135         WEnd
3136         '
3137         '
3138         '
3139         '----- 検査実行 -----
3140         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
3141             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
3142                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
3143                 MInspRetryExitFlg = 0
3144                 MRetryCnt = 2                                        'Retry回数設定
3145                 While( MInspRetryExitFlg = 0 )
3146                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
3147                     '
3148                     '検査完了確認
3149                     MRetryCnt = MRetryCnt - 1
3150                     M_Timer(1) = 0
3151                     MExitFlg = 0
3152                     While( MExitFlg = 0 )
3153                     '検査完了待ち
3154                         '検査OK終了?
3155                         If M_In( MIN_IS_InspOK% ) = 1  Then
3156                             MJudgeOKFlg = 1                         '検査OKフラグON
3157                             MExitFlg = 1
3158                         '
3159                         '検査NG終了?
3160                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
3161                             If MInspErrNum = 0 Then                 '1回目のエラー?
3162                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
3163                                     MInspErrNum = 32                    '検査NG エラー番号=32
3164                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
3165                                 EndIf
3166                             EndIf
3167                             MExitFlg = 1
3168                         '
3169                         '検査異常終了(IS timeout)?
3170                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
3171                             If MInspErrNum = 0 Then                 '1回目のエラー?
3172                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
3173                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
3174                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
3175                                 EndIf
3176                             EndIf
3177                             MExitFlg = 1
3178                         '
3179                         'timeoutチェック
3180                         ElseIf 3000 < M_Timer(1) Then
3181                             If MInspErrNum = 0 Then                 '1回目のエラー?
3182                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
3183                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
3184                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
3185                                 EndIf
3186                             EndIf
3187                             MExitFlg = 1
3188                         EndIf
3189                     WEnd
3190                     '
3191                     '検査開始要求off
3192                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
3193                     '
3194                     'OKなら抜ける
3195                     If MJudgeOKFlg = 1 Then
3196                         MInspRetryExitFlg = 1
3197                     Else
3198                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
3199                         If MRetryCnt = 0 Then
3200                             MInspRetryExitFlg = 1
3201                         Else
3202                             'Retryへ　その前にDelay
3203                             Dly 0.3
3204                         EndIf
3205                     EndIf
3206                     '
3207                 WEnd
3208             EndIf
3209         EndIf
3210         '
3211         '
3212         '
3213         MNum% = MNum% + 1                                           '検査Step+1
3214         '検査終了確認　検査終了フラグセット
3215         If (MInspCnt% < MNum% ) Then
3216             MEndFlg% = 1                                            '検査終了フラグセット
3217         EndIf
3218         'NG発生時続行時処理
3219         If MInspErrNum <> 0 Then                                    'NGあり?
3220             If MNgContinue% <> 1 Then                               'NG続行?
3221                 MEndFlg% = 1                                        '検査終了フラグセット
3222             EndIf
3223         EndIf
3224     WEnd
3225     '
3226     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
3227     If 0 < MZAxis% Then
3228         PCurrentPos = P_Curr                                        '現在位置取得
3229         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
3230         fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
3231         Mvs PCurrentPos                                             '現在位置上空へ移動
3232     EndIf
3233     '
3234     '戻り値設定
3235     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
3236         ISInspectionSingle = 1                                      '正常終了戻り値設定
3237     Else
3238         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
3239         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
3240         ISInspectionSingle = 0                                      '異常終了戻り値設定
3241     EndIf
3242     Fine 0 , P
3243     Exit Function
3244 FEnd
3245 '
3246 '■InitialZoneB
3247 ''' <summary>
3248 ''' 非常停止後の復帰動作
3249 ''' 1)上空退避　Z方向上に移動
3250 ''' 2)J1軸以外を退避ポジションへ移動
3251 ''' 3)J1軸のみを退避ポジションへ移動
3252 ''' 4)イニシャルポジションへ移動
3253 ''' </summary>
3254 ''' <remarks>
3255 ''' Date : 2022/03/23 : N.Watanabe
3256 ''' </remarks>
3257 Function V fnInitialZoneB()
3258     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/05/09 渡辺
3259 'パラメータ
3260     Ovrd 5
3261 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
3262 '    Cmp Pos, &B100011
3263 '
3264 '復帰動作開始
3265 'ハンドをイニシャルに戻す
3266 '    M_Out(12256) = 0    '本体チャック閉OFF
3267 '    M_Out(12257) = 1    '本体チャック開ON
3268 '    Dly 1.0
3269 '上空退避
3270     PActive = P_Curr
3271     Pmove = PActive
3272     Pmove.Z = 600           '上空退避する一律の高さ
3273     If PActive.X > 400 Then
3274         Pmove.Z =400        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
3275     EndIf
3276     If PActive.Z < Pmove.Z Then '現在の高さがPmoveより低い時のみ実行
3277         Mvs Pmove
3278     EndIf
3279     Dly 1.0
3280 'J1軸以外を退避ポジションへ移動
3281     JActive = J_Curr
3282     Jmove = JTaihi
3283     Jmove.J1 = JActive.J1        'J1軸のみ現在値を使用し、他の軸はJTaihiのポーズを取る
3284     Mov Jmove
3285     Dly 1.0
3286 'J1軸のみを退避ポジションへ移動
3287     Mov JTaihi
3288     Dly 1.0
3289 'イニシャルポジションへ移動
3290     Mov PInitialPosition
3291     Cmp Off
3292     Ovrd 100
3293     M_Out(12268) = 0            '位置決め出OFF
3294     M_Out(12269) = 1            '位置決め戻ON
3295     fErrorProcess(11,253,281,0)
3296     Exit Function
3297 FEnd
3298 '
3299 '
3300 '■fnAutoScreenComment
3301 ''' <summary>
3302 ''' メイン画面の動作状況表示
3303 ''' コメントD1005の設定
3304 ''' </summary>
3305 '''<param name="McommentD1005%">コメントID</param>
3306 ''' <remarks>
3307 ''' Date   : 2021/07/07 : M.Hayakawa
3308 ''' </remarks>
3309 Function fnAutoScreenComment(ByVal McommentD1005%)
3310     M_Out16(12576) = McommentD1005%
3311     Exit Function
3312 FEnd
3313 '
3314 '■fnRoboPosChk
3315 ''' <summary>
3316 ''' 最後に終了したロボットポジションの確認
3317 ''' </summary>
3318 '''<param name="MINNumber%">入力番号</param>
3319 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3320 '''<param name="MTimeCnt&">タイムアウト時間</param>
3321 ''' PLCに保続した番号を読込み、確認
3322 ''' MRBTOpeGroupNo = 5 が初期位置に設定
3323 '''<returns>整数 0:タイムアウト 1:OK</returns>
3324 ''' <remarks>
3325 ''' Date   : 2021/07/07 : M.Hayakawa
3326 ''' </remarks>
3327 Function M% fnRoboPosChk
3328     fnRoboPosChk = 0
3329     MRet = fnStepRead()
3330     '初期位置でないと判断した場合
3331     'ウィンド画面切換え
3332     If MRBTOpeGroupNo > 5 Then
3333         '下記キー待ちの継続に反応させないため
3334         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3335         Dly 0.2
3336         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3337         Dly 1.5
3338         '
3339         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
3340         '
3341         MLoopFlg% = 1
3342         While MLoopFlg% = 1
3343             '
3344             '
3345             MKeyNumber% = fnKEY_WAIT()
3346             Select MKeyNumber%
3347                 Case Is = MAbout%       '停止
3348                     M_20# = MAbout%
3349                     MLoopFlg% = -1
3350                     Break
3351                 Case Is = MNext%        '次へ
3352                     'MLoopFlg% = -1
3353                     Break
3354                 Case Is = MContinue%    '継続
3355                     M_20# = MContinue%
3356                     MLoopFlg% = -1
3357                     Break
3358                 Default
3359                     Break
3360             End Select
3361         WEnd
3362     EndIf
3363     '
3364     If M_20# = MContinue% Then                              '継続ボタンが押された場合
3365         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
3366         Ovrd 5                                   '低速オーバーライド値設定
3367         Select MRBTOpeGroupNo
3368             Case Is = 5                          '何もしない
3369                 Break
3370             Case Is = 10                         '初期位置へ戻す
3371                 'Mov PTEST001
3372                 Break
3373             Case Is = 15                         '初期位置へ戻す
3374                 'Mov PTEST002
3375                 Dly 0.5
3376                 'Mov PTEST001
3377                 Dly 0.5
3378                 Break
3379             Default
3380                 Break
3381         End Select
3382         '
3383         Ovrd M_NOvrd                            'システムの初期値を設定
3384         M_Out(12364) = 1                        'toPLC_データ保存ON
3385         MRBTOpeGroupNo = 5
3386         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
3387         Dly 1.0
3388         M_Out(12364) = 0                        'toPLC_データ保存OFF
3389         fnRoboPosChk = 1                        '初期位置動作実行
3390         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
3391     EndIf
3392     Exit Function
3393 FEnd
3394 '
3395 '■frInCheck
3396 ''' <summary>
3397 ''' センサーINチェック
3398 ''' </summary>
3399 '''<param name="MINNumber%">入力番号</param>
3400 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3401 '''<param name="MTimeCnt&">タイムアウト時間</param>
3402 '''<returns>整数 0:タイムアウト 1:OK</returns>
3403 ''' <remarks>
3404 ''' Date   : 2021/07/07 : M.Hayakawa
3405 ''' </remarks>
3406 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3407     M_Timer(4) = 0
3408     MloopFlg = 0
3409     While MloopFlg = 0
3410         MCrtTime& = M_Timer(4)
3411         If M_In(MINNumber%) = MCMPFLG% Then
3412             MloopFlg = 1
3413             frInCheck = 1
3414         ElseIf MCrtTime& > MTimeCnt& Then
3415             MloopFlg = 1
3416             frInCheck = 0
3417         EndIf
3418     WEnd
3419     Exit Function
3420 FEnd
3421 '-----------------------------------------------
3422 '
3423 'ねじ締め機通信確認
3424 '
3425 '-----------------------------------------------
3426 Function M% fScewTcomChk
3427     fScewTcomChk = 0
3428     '通信確認送信
3429     M_Out(MOUT_ScwT_ComChk%) = MOn%
3430     '通信確認受信待機
3431     Wait M_In(MIN_ScwT_comOK%) = MOn%
3432     '通信確認送信終了
3433     M_Out(MOUT_ScwT_ComChk%) = MOff%
3434     Exit Function
3435 FEnd
3436 '
3437 '
3438 '-----------------------------------------------
3439 '
3440 'ねじ締め開始送信
3441 '
3442 '-----------------------------------------------
3443 Function M% fScewTStart
3444     fScewTStart = 0
3445     'ねじ締め開始待機を受信
3446     Wait M_In(MIN_ScwT_STRec%) = MOn%
3447     Dly 0.1
3448     'ねじ締め開始受信を送信
3449     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
3450     Exit Function
3451 FEnd
3452 '
3453 '
3454 '-----------------------------------------------
3455 '
3456 'ねじ締め完了受信
3457 '
3458 '-----------------------------------------------
3459 Function M% fScewTFinish
3460     fScewTFinish = 0
3461     'ねじ締め完了待機を受信
3462     Wait M_In(MIN_ScwT_Fin%) = MOn%
3463     Dly 0.1
3464     'ねじ締め完了受信を送信
3465     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
3466     Exit Function
3467 FEnd
3468 '
3469 '
3470 '-----------------------------------------------
3471 '
3472 '条件xx停止受信
3473 '
3474 '-----------------------------------------------
3475 Function M% fScewTCaseStop(ByVal MCase%())
3476     fScewTCaseStop = 0
3477     '条件xx停止を受信
3478     Wait M_In(MCase%(1)) = MOn%
3479     Dly 0.1
3480     '条件xx停止受信を送信
3481     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
3482     Exit Function
3483 FEnd
3484 '
3485 '-----------------------------------------------
3486 '
3487 '再開始受信
3488 '
3489 '-----------------------------------------------
3490 Function M% fScewTReStart()
3491     fScewTReStart = 0
3492     '再開始を受信
3493     Wait M_In(MIN_ScwT_ReST%) = MOn%
3494     Dly 0.1
3495     '再開始受信を送信
3496     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
3497     Exit Function
3498 FEnd
3499 '
3500 '■fErrorProcess
3501 '<summary>
3502 'エラー処理
3503 '</summary>
3504 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3505 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3506 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3507 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3508 '<make>
3509 '2021/11/5 中村天哉
3510 '</make>
3511 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3512     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3513     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3514     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3515     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3516 *RETRY_ERR_PROCESS
3517      M_20# = MClear%     '初期化
3518 '        'エラー処理記述
3519         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3520 '        'GOT KEY入力待ち
3521         MKeyNumber = fnKEY_WAIT()
3522 '        '
3523         If MKeyNumber = MAbout% Then   '停止を選択した場合
3524             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3525             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3526             Break
3527          '
3528         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3529             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3530             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3531         '
3532         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3533             M_20# = MNext%            'M_20# プログラム間共通外部変数
3534             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3535          '
3536         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3537             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3538             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3539             Break
3540         '
3541         EndIf
3542         '
3543         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3544     Exit Function
3545 FEnd
3546 '
3547 '■fnTorqueCheck
3548 ''' <summary>
3549 ''' トルクチェック動作用のメイン
3550 ''' </summary>
3551 ''' <remarks>
3552 ''' Date   : 2021/12/21 : H.AJI
3553 ''' </remarks>'
3554 Function M% fnTorqueCheck
3555     'トルクチェック中送信  搬送系停止
3556     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3557     '
3558     fnTorqueCheck = 0
3559     Ovrd 20
3560     Mov PInitialPosition              '初期位置移動
3561     Ovrd 100
3562     '下記キー待ちの継続に反応させないため
3563     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3564     Dly 0.2
3565     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3566     '
3567     'M6340  トルクチェック受信
3568     'Dly 5.0
3569     M_Out(12340) = 1          'トルクチェック受信 M6340
3570     Dly 1.0
3571     M_Out(12340) = 0
3572     '
3573     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3574     '
3575     MLoopFlg = 1
3576     While MLoopFlg = 1
3577         '
3578         Mov PInitialPosition              '初期位置移動
3579         '
3580         MKeyNumber = fnKEY_WAIT()
3581         Select MKeyNumber
3582             Case Is = 1           '停止
3583                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3584                 Dly 1.0
3585                 M_Out(12343) = 0
3586                 Ovrd 20
3587                 Mov PTicketRead_1
3588                 Ovrd 100
3589                 M_20# = 1
3590                 MLoopFlg = -1
3591                 Break
3592             Case Is = 2           '次へ
3593                 Break
3594             Case Is = 3           '継続
3595                 Break
3596             Case Is = 4           'トルクチェック開始
3597                 M_Out(12545) = 1    ' toPLC_PCトルクチェック1要求受信(M315)
3598                 M_Out(12342) = 1 Dly 1.0    'トルクチェック開始要求受信 M6342
3599                 fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3600                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3601                 MRet = fnMoveTorquePosi()
3602                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3603                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3604                 Break
3605             Default
3606                 Break
3607         End Select
3608     WEnd
3609     '
3610     'トルクチェック中停止送信
3611     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3612     '
3613     'ロボットの位置を元に戻す
3614     '
3615     Exit Function
3616  FEnd
3617  '
3618 '
3619 '
3620 '---------------------------
3621 '
3622 '    メイン画面の表示、非表示設定
3623 '         コメントD1001, D1002, D1003の設定
3624 '           MWindReSet = 0     画面非表示
3625 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3626 '           MWindErrScr = 10    エラー画面 D1001, D1002
3627 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3628 '
3629 '---------------------------
3630 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3631     fnMainScreenOpen = 0
3632     '
3633    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3634         M_Out16(12480) = MCommentD1001            'D1001 コメント
3635     EndIf
3636     '
3637     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3638         M_Out16(12496) = MCommentD1002            'D1002 コメント
3639     EndIf
3640     '
3641     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3642         M_Out16(12512) = MCommentD1003            'D1003 コメント
3643     EndIf
3644     '
3645     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3646     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3647     Dly 0.5
3648     M_Out(12362) = 0                         'ウィンド画面設定
3649     Exit Function
3650 FEnd
3651 '
3652 '■Main
3653 ''' <summary>
3654 ''' トルクチェック実動作
3655 ''' </summary>
3656 ''' <remarks>
3657 ''' Date   : 2021/12/21 : H.AJI
3658 ''' </remarks>'
3659 Function M% fnMoveTorquePosi
3660      fnMoveTorquePosi = 0
3661      Ovrd 50
3662      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
3663     '
3664     Spd M_NSpd
3665 '-------------      ドライバーRST
3666     M_Out(12240)=0     'ドライバーOFF CCW
3667     M_Out(12241)=0     'ドライバーOFF CW
3668     M_Out(12242)=0     'ドライバー解除 C1
3669     M_Out(12243)=0     'ドライバー解除 C2
3670     M_Out(12245)=0     'プログラム解除 F1/プログラム2
3671 '---------------------------------------
3672 '[P-11]
3673 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
3674     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
3675     Dly 0.1
3676 '-----------------------
3677    'Cnt 0                           'Cnt動作-2　終了
3678 '-----------------------
3679     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
3680     Dly 0.2
3681 '-----------------------
3682     ProgramBankSet(1,3)
3683     M_Out(12241)=0                   'ドライバーOFF  CW
3684     'Dly 0.1
3685 '--------------------------------
3686     Ovrd 40
3687    'Dly 0.1
3688 '--------------------------------  ネジ締め速度設定
3689     Spd 14                            'ライド 100-40 100% :Spd 12
3690     Dly 0.1
3691 '--------------------------------
3692 '--------------------------------
3693 '---------------------------------【ねじ締め動作】
3694 '
3695     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
3696    Mvs PTorqueCheck               'トルクチェック位置へ移動
3697     Dly 0.3                          '動作安定待ち
3698    M_Out(12241)=1                   'ドライバーON  CW
3699 '
3700     Wait M_In(11584)=1                '完了/エラー検出
3701     Dly 0.1
3702     Spd M_NSpd
3703    'Ovrd 20
3704     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
3705     Wait M_In(11257)=1                'ネジ完了SC
3706 '---------------------------------
3707     Dly 0.1
3708     M_Out(12241)=0                    'ドライバーOFF CW
3709     Dly 0.1
3710     M_Out(12242)=0                    'ドライバー解除 C1
3711     Dly 0.1
3712     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
3713     Dly 0.1
3714     M_Out(12245)=0                    'プログラム2解除 F1
3715 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
3716 '
3717     Mvs PTorqueCheck,-60                       'あえてmov から変更
3718     Dly 0.1
3719 '--------------------------------------------------------------
3720    'Ovrd 80
3721 '--------------------------------------------------------------
3722 '---------------------------------------
3723 '---------------------------------------
3724 '---------------------------------------エラー離脱処理
3725    *LBL1
3726    Fsc Off            '力覚センサ　Off   *STEP1は不要
3727    Mvs ,-100
3728    M_Out(12241)=0     'ドライバーOFF CW
3729    Dly 0.1
3730    M_Out(12242)=0     'ドライバー解除 C1
3731    Dly 0.1
3732    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
3733    Dly 0.1
3734    M_Out(12245)=0     'プログラム解除 F1
3735 '---------------------------------------
3736 '---------------------------------------
3737 '-------------
3738    'Mov PInitPos19049
3739    Dly 0.1
3740 '
3741 '
3742     Exit Function
3743 FEnd
3744 '
3745 '■Main
3746 ''' <summary>
3747 ''' 組立動作用のメイン
3748 ''' </summary>
3749 ''' <remarks>
3750 ''' Date   : 2021/07/07 : M.Hayakawa
3751 ''' </remarks>'
3752 Function Main
3753     MopeNo = M_21#         '外部変数にて動作番号代入
3754     '
3755     If M_Svo=0 Then
3756         Servo On
3757     EndIf
3758     Wait M_Svo=1
3759 '組立スタート日付時刻要求パルスON
3760     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3761 'パトライト操作
3762     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3763     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3764     '
3765     M_20# = 0                                   'KEY入力初期化
3766     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3767     MRet% = 0
3768 '初期位置の確認と移動
3769 '
3770 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
3771     PActive = P_Curr                    '現在位置を取得
3772     MRecoveryPass% = 0
3773     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3774         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3775             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3776                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3777             EndIf
3778         EndIf
3779     EndIf
3780     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3781         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3782             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3783                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3784             EndIf
3785         EndIf
3786     EndIf
3787     If MRecoveryPass% = 0 Then
3788        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3789     EndIf
3790 '
3791 '
3792 '    MRet% = fnRoboPosChk()
3793     If MRet% = 1 Then                           '初期位置の動作を行った場合
3794         fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3795         MKeyNumber% = fnKEY_WAIT()
3796         Select MKeyNumber%
3797             Case Is = MAbout%       '停止
3798                 M_20# = MAbout%
3799                 MLoopFlg% = -1
3800                 Break
3801             Case Is = MNext%        '次へ
3802                 'MLoopFlg = -1
3803                 Break
3804             Case Is = MContinue%    '継続
3805                 M_20# = MContinue%
3806                 MLoopFlg% = -1
3807                 Break
3808             Default
3809                 Break
3810         End Select
3811     EndIf
3812     '
3813     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3814         M_Out(12364) = 1            'toPLC_データ保存ON
3815 'トルクチェック
3816         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3817             MRet% = fnTorqueCheck()
3818             Break
3819         Else
3820 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3821 '                MRtn = InspInit()               '画像処理初期化処理
3822 '            EndIf
3823             '
3824            M_20# = MClear%                    '初期化
3825 '組立開始
3826             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3827 '                MRet% = fnAssyStart()
3828                 fnAssyStart()
3829             Else
3830                 M_20# = MPass%
3831             EndIf
3832 '組立終了日付時刻
3833             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3834             Wait M_In(11572) = 1            '日付取得完了
3835             Dly 0.1
3836             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3837 'リフターユニットへのOUT
3838             '  KEY入力が何もない場合 OKと判断
3839             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3840             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3841 'OK/NGフラグ出力
3842             If M_20# <= 0 Then
3843                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3844             ElseIf M_20# = MPass% Then
3845                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3846             EndIf
3847 'PIASに組立完了書込み
3848             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3849                 If M_20# = MPass% Then
3850                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3851                 Else
3852                     'KEY入力がNGの場合
3853                     If M_20# = MNgProcess% Then
3854                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3855                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3856                         MRet% = fnPiasWrite(MNG%)
3857                        nAssyNgQty = nAssyNgQty + 1
3858                     EndIf
3859                     '
3860                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3861                     If M_20# = MAssyOK% Then
3862                             '-----------------------
3863                             'D732 -> D2600 コピー要求
3864                             M_Out(12566) = 1
3865 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3866                             M_Out(12566) = 0
3867                             '
3868                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3869                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3870                             '基板番号照合(PPは未使用）
3871 '                            MRet% = fnPCBNumberCheck()
3872                         Else
3873                             MRet% = 1
3874                         EndIf
3875                         '
3876                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3877                             If M_20# <> MAbout% Then
3878                                 '工程履歴OK書き込み
3879                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3880                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3881                                 MRet% = fnPiasWrite(MOK%)
3882                                 nAssyOkQty = 0
3883                                 nAssyOkQty = nAssyOkQty + 1
3884                             Else
3885                                 nAssyOkQty = nAssyOkQty + 1
3886                             EndIf
3887                         EndIf
3888                     EndIf
3889 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3890 '                    MRet% = fnPiasWrite(MOK%)
3891                 EndIf
3892             Else
3893                 nAssyOkQty = nAssyOkQty + 1
3894             EndIf
3895             '
3896             '組立終了日付時刻解除
3897             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3898             '投入数、組立OK数、組立NG数書込み
3899 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3900             '
3901 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3902 '                '画像処理終了処理
3903 '                MRtn = InspQuit()
3904 '            EndIf
3905         EndIf
3906         M_Out(12364) = 0                          'toPLC_データ保存OFF
3907     EndIf
3908 'パトライト操作
3909     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3910     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3911 'GOT表示
3912     fnAutoScreenComment(93)  'AUTO画面 工程完了
3913 FEnd
3914 End
3915 '
3916 'おまじないコメント
3917 '絶対削除するな
3918 '
3919 '
3920 '
3921 '
JActive=(163.570,10.110,83.330,0.000,86.560,-16.430,0.000,0.000)
Jmove=(163.570,-44.940,112.380,0.000,76.270,0.000,0.000,0.000)
JTaihi=(0.000,-44.940,112.380,0.000,76.270,0.000,0.000,0.000)
PActive=(601.260,-152.240,450.960,180.000,0.000,90.000,0.000,0.000)(7,0)
PEscapePosition=(247.610,-0.380,580.000,-180.000,0.000,-179.990)(7,0)
PEscapePosition_2=(-131.770,177.640,579.990,-180.000,0.000,-53.320)(7,0)
PEscapePosition_3=(-178.950,226.160,579.830,-180.000,-0.020,-53.330)(7,0)
PEscapePosition_4=(-263.930,0.030,580.000,-180.000,0.000,0.100)(7,0)
PInitialPosition=(300.000,0.000,440.000,-180.000,0.000,-180.000)(7,0)
Pmove=(-375.800,110.800,600.000,180.000,0.000,0.000,0.000,0.000)(7,0)
PProductOnJigGet=(-243.200,-0.160,427.020,30.910,88.850,-149.690)(6,1)
PProductOnJigGet_1=(-243.200,-0.160,460.000,30.910,88.850,-149.690)(6,1)
PProductOnJigGet_2=(-190.000,-0.160,559.980,37.460,88.990,-143.250)(6,1)
PProductOnJigGet_3=(-135.390,133.310,559.950,38.070,89.000,172.380)(6,0)
PProductOnJigGet_4=(-164.650,0.000,671.530,-166.660,90.000,13.340)(7,0)
PProductOnJigGet_5=(-224.300,0.010,604.770,180.000,0.000,0.000)(7,0)
PProductOnJigGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnJigSet=(-243.200,-0.160,427.020,30.910,88.850,-149.690)(6,1)
PProductOnJigSet_1=(-243.200,-0.160,460.000,30.910,88.850,-149.690)(6,1)
PProductOnJigSet_2=(-190.000,-0.160,559.990,37.460,88.980,-143.250)(6,1)
PProductOnJigSet_3=(-137.460,131.730,570.000,-172.800,89.910,-36.530)(6,0)
PProductOnJigSet_4=(139.330,129.750,570.000,-172.930,89.910,-129.920)(6,0)
PProductOnPltGet=(546.010,-99.240,250.980,-179.820,0.470,179.730)(7,0)
PProductOnPltGet_1=(546.010,-99.240,290.000,-179.820,0.470,179.730)(7,0)
PProductOnPltGet_2=(546.010,-99.240,400.000,-179.820,0.470,179.730)(7,0)
PProductOnPltSet=(544.890,-99.240,250.480,-179.820,0.470,179.730)(7,0)
PProductOnPltSet_1=(544.890,-99.240,290.000,-179.820,0.470,179.730)(7,0)
PProductOnPltSet_2=(544.890,-99.240,400.000,-179.820,0.470,179.730)(7,0)
PProductOnPltSet_3=(139.330,129.750,570.000,-172.920,89.910,-129.910)(6,0)
PScrewHeatSink1=(-376.000,84.190,567.280,-180.000,0.000,0.000)(7,0)
PScrewHeatSink1_0=(-376.000,84.190,575.780,-180.000,0.000,0.000)(7,0)
PScrewHeatSink1_1=(-376.000,84.190,620.000,-180.000,0.000,0.000)(7,0)
PScrewHeatSink2=(-375.800,110.800,567.190,-180.000,0.000,0.000)(7,0)
PScrewHeatSink2_0=(-375.800,110.800,575.990,180.000,0.000,0.000)(7,0)
PScrewHeatSink2_1=(-375.800,110.800,620.000,-179.990,0.050,0.000)(7,0)
PScrewPlateL=(-284.780,112.540,536.900,-180.000,0.000,-90.000)(7,0)
PScrewPlateL1=(-314.190,29.000,536.160,-180.000,0.000,-90.000)(7,0)
PScrewPlateL1_0=(-314.190,29.000,542.000,180.000,0.000,-90.000)(7,0)
PScrewPlateL1_1=(-314.190,29.000,570.000,180.000,0.000,-90.000)(7,0)
PScrewPlateL2=(-314.330,103.970,535.830,180.000,0.000,-90.000)(7,0)
PScrewPlateL2_0=(-314.330,103.970,540.950,180.000,0.000,-90.000)(7,0)
PScrewPlateL2_1=(-314.330,103.970,570.000,180.000,0.000,-90.000)(7,0)
PScrewPlateL_0=(-284.780,112.540,543.530,-180.000,0.000,-90.000)(7,0)
PScrewPlateL_1=(-284.780,112.540,570.000,-180.000,0.000,-90.000)(7,0)
PScrewPlateR=(-286.120,-114.330,536.390,-180.000,0.000,-90.000)(7,1)
PScrewPlateR1=(-314.100,-26.710,535.810,-180.000,0.000,-89.940)(7,1)
PScrewPlateR1_0=(-314.100,-26.710,541.970,180.000,0.000,-89.940)(7,1)
PScrewPlateR1_1=(-314.100,-26.710,570.000,180.000,0.000,-89.940)(7,1)
PScrewPlateR2=(-315.100,-101.790,536.270,-179.990,0.000,-89.940)(7,1)
PScrewPlateR2_0=(-315.100,-101.790,541.210,-179.990,0.000,-89.940)(7,1)
PScrewPlateR2_1=(-315.100,-101.790,570.000,-179.990,0.000,-89.940)(7,1)
PScrewPlateR_0=(-286.120,-114.330,543.600,-180.000,0.000,-90.000)(7,1)
PScrewPlateR_1=(-286.120,-114.330,570.000,-180.000,0.000,-90.000)(7,1)
PScrewSupplyHS=(-281.270,-239.640,400.970,-179.990,0.000,-58.700)(7,1)
PScrewSupplyHS_1=(-281.270,-239.640,422.840,-179.990,0.000,-58.700)(7,1)
PScrewSupplyHS_2=(-147.310,-147.280,610.000,-180.000,0.000,-45.010)(7,1)
PScrewSupplyHS_3=(-235.180,-0.020,609.980,180.000,0.000,0.000)(7,1)
PScrewSupplyHS_4=(-271.460,-216.690,422.800,-180.000,0.000,-90.000)(7,1)
PScrewSupplyHS_5=(-164.650,0.000,671.530,-166.660,90.000,13.340)(7,0)
PScrewSupplyPlate=(-134.450,198.940,450.150,-180.000,0.010,170.740)(7,0)
PScrewSupplyPlate_1=(-134.450,198.940,470.000,180.000,0.010,170.740)(7,0)
PScrewSupplyPlate_2=(-135.250,198.940,570.000,-180.000,0.010,170.740)(7,0)
PScrewSupplyPlate_3=(-132.530,160.720,570.000,-179.990,0.000,-140.430)(7,0)
PScrewSupplyPlate_4=(-208.310,0.230,610.000,-180.000,0.000,-90.000)(7,0)
PScrewSupplyPlate_5=(-113.970,113.990,696.720,-0.320,89.240,134.680)(7,0)
PScrewSupplyPlate_6=(-161.190,0.020,696.720,-0.320,89.240,179.670)(7,0)
PScrewSupplyPlate_7=(-50.580,239.350,544.690,-180.000,0.010,170.740)(7,0)
PScrewSupplyPlatel_2=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PTemp=(601.260,-152.240,450.960,180.000,0.000,90.000,0.000,0.000)(7,0)
PTicketRead=(601.260,-152.240,374.960,-180.000,0.000,90.000)(7,0)
PTicketRead_1=(601.260,-152.240,450.960,-180.000,0.000,90.000)(7,0)
PTorqueCheck=(148.860,-273.360,336.080,-180.000,-0.010,110.020)(7,0)
PTorqueCheck_1=(148.840,-273.370,360.500,-180.000,-0.010,110.020)(7,0)
PEscapePosi(1)=(-252.420,-263.310,570.000,179.960,-0.070,-43.810)(7,1)
PEscapePosi(2)=(-141.510,-152.870,570.000,179.960,-0.070,-43.810)(7,1)
PEscapePosi(3)=(-208.310,0.230,570.000,-180.000,0.000,-90.000)(7,0)
PEscapePosi(4)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(5)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(6)=(-208.310,0.010,610.000,180.000,0.000,0.000)(7,0)
PEscapePosi(7)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(8)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(9)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(10)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PGetScrewPos(1)=(-134.450,198.940,470.000,180.000,0.010,170.740,0.000,0.000)(7,0)
PGetScrewPos(2)=(-135.250,198.940,570.000,-180.000,0.010,170.740,0.000,0.000)(7,0)
PGetScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(9)=(-50.580,239.350,544.690,-180.000,0.010,170.740,0.000,0.000)(7,0)
PGetScrewPos(10)=(-134.450,198.940,450.150,-180.000,0.010,170.740,0.000,0.000)(7,0)
PInspPosition(1)=(601.260,-152.240,374.960,-180.000,0.000,90.000,0.000,0.000)(7,0)
PInspPosition(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(11)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(12)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(13)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(14)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(15)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(16)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(17)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(18)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(19)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(20)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(21)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(22)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(23)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(24)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(25)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(26)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(27)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(28)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(29)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(30)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(1)=(-315.100,-101.790,570.000,-179.990,0.000,-89.940,0.000,0.000)(7,1)
PScrewPos(2)=(-315.100,-101.790,541.210,-179.990,0.000,-89.940,0.000,0.000)(7,1)
PScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(10)=(-315.100,-101.790,536.270,-179.990,0.000,-89.940,0.000,0.000)(7,1)
