<!-- tabs:start -->

#### **Documentation**

# Documentation for `Mconsignee` Code Unit

## 1. Overview:

### Objective:
The `Mconsignee` code unit is designed to manage and maintain consignee-related data within a user interface. It provides a form-based interface for users to view, edit, and manage consignee details, including associated data such as addresses, contacts, delivery days, vehicle types, and more. The form also supports actions like duplicating records, printing current records, and navigating through detailed tabs for specific consignee-related information.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the graphical user interface and handling events.
- **Third-party Components**: Includes components like `TsPanel`, `TsSplitter`, `TsBitBtn`, and `TsPageControl` for enhanced UI/UX.
- **Database Interaction**: Uses `DBClient` for managing data connections and operations.
- **Custom Frames**: Includes custom frames like `FRAMEconsignee`, `FRAMElistAddresses`, and others for modular UI design.

### Form Type:
This is a **form-based interface** with the following elements:
- **Form Elements**:
  - Panels (`TsPanel`)
  - Buttons (`TsBitBtn`, `TsSpeedButton`)
  - Splitters (`TsSplitter`)
  - Tab Control (`TsPageControl` with multiple tabs)
  - Custom Frames for specific functionalities
- **Form Actions**:
  - Duplicate a record
  - Print the current record
  - Add new records
  - Cancel operations
  - Navigate through tabs to view/edit specific details

---

## 2. Functionality Description:

### User/Software Actions:
- View and edit consignee details.
- Duplicate existing consignee records.
- Print the current record.
- Add new consignee-related data (e.g., delivery days, addresses).
- Navigate through tabs to manage specific consignee-related information.

### Main Components:
1. **Panels (`TsPanel`)**: Organize the layout and group related controls.
2. **Buttons (`TsBitBtn`, `TsSpeedButton`)**: Trigger actions like duplication, printing, and navigation.
3. **Tab Control (`TsPageControl`)**: Provides a tabbed interface for managing different aspects of consignee data.
4. **Custom Frames**: Modular components for specific functionalities (e.g., `FRAMEconsignee`, `FRAMElistAddresses`).

### Pseudo-code for Actions and Events:
- `OnClick` event of `BTprintCurrentRecord`: `if button clicked then print current record`.
- `OnClick` event of `FRAMEconsDelDay1BTNadd`: `if button clicked then add new delivery day`.
- `OnExecute` event of `ACTduplicate`: `if action executed then duplicate current record`.
- `OnClick` event of `BTNew`: `if button clicked then create new record`.
- `OnTimer` event of `TMRstdLogCost`: `if timer triggered then execute standard logistic cost logic`.
- `OnClick` event of `BTCancel`: `if button clicked then cancel operation`.
- `OnChange` event of `PGCdetails`: `if tab changed then load corresponding data`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized, and UI components are loaded.
   - Data is fetched and displayed in the respective fields and tabs.

2. **User Interactions**:
   - Users can interact with buttons, tabs, and other controls to perform actions like duplication, printing, and data entry.

3. **Triggered Functions**:
   - `FormShow`: Initializes the form and loads data.
   - `BTprintCurrentRecordClick`: Prints the current record.
   - `FRAMEconsDelDay1BTNaddClick`: Adds a new delivery day.
   - `ACTduplicateExecute`: Duplicates the current record.
   - `BTNewClick`: Creates a new record.
   - `TMRstdLogCostTimer`: Handles timer-based logic for standard logistic costs.
   - `BTCancelClick`: Cancels the current operation.
   - `PGCdetailsChange`: Handles tab change events.

### Required Data:
- Consignee details (e.g., name, address, contact information).
- Delivery day information.
- Vehicle type and special device details.
- Logistic cost data.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Duplicate Button**: Enabled only if the user has the necessary permissions (`FcanDuplicate`).
- **Print Button**: Enabled when a record is selected.
- **Add Button**: Enabled when the user is on the delivery day tab.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Not explicitly defined in the code.

---

## 5. Main Functions:

1. **`m_getData`**: Fetches data to populate the form.
2. **`m_Validate`**: Validates the form data before saving.
3. **`m_PutData`**: Saves the form data to the database.
4. **`m_CreateFormEdit`**: Creates and initializes the form.
5. **`SetFormState`**: Configures the form's state based on access mode.
6. **`SetFrameState`**: Configures the state of specific frames.

---

## 6. API Service Consumption:

No explicit API calls are defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **Delphi VCL Components**: For UI and event handling.
- **Third-party Components**: Includes `TsPanel`, `TsSplitter`, `TsBitBtn`, etc.

### Custom Components:
- **Custom Frames**: `FRAMEconsignee`, `FRAMElistAddresses`, `FRAMEconsDelDay`, etc.

---

## 9. Fields and Validations Listing:

1. **Country**
   - **Caption**: `Country`
   - **Control**: `FRAMEfindCountry`
   - **Focus Control**: `FRAMEfindCountry.DBE`

2. **State**
   - **Caption**: `State`
   - **Control**: `FRAMEfindState`
   - **Focus Control**: `FRAMEfindOrdDest.DBE`

3. **Market**
   - **Caption**: `Market`
   - **Control**: `FRAMEfindConsMarket`
   - **Focus Control**: `FRAMEfindConsMarket.DBE`

4. **Warehouse**
   - **Caption**: `Warehouse`
   - **Control**: `FRAMEfindWarehouse`
   - **Focus Control**: `FRAMEfindWarehouse.DBE`

5. **Delivery Terms**
   - **Caption**: `Delivery Terms`
   - **Control**: `FRAMEfindDelTerm`
   - **Focus Control**: `FRAMEfindDelTerm.DBE`

6. **Del Policy**
   - **Caption**: `Del Policy`
   - **Control**: `FRAMEfindDelPolicy`
   - **Focus Control**: `FRAMEfindDelPolicy.DBE`

7. **Syst. of Measure**
   - **Caption**: `Syst. of Measure`
   - **Control**: `FRAMEfindUnitSys`
   - **Focus Control**: `FRAMEfindUnitSys.DBE`

8. **Destination**
   - **Caption**: `Destination`
   - **Control**: `FRAMEfindDestination`
   - **Focus Control**: `FRAMEfindDestination.DBE`

9. **Language**
   - **Caption**: `Language`
   - **Control**: `FRAMEfindLanguage`
   - **Focus Control**: `FRAMEfindLanguage.DBE`

10. **Ord Dest**
    - **Caption**: `Ord Dest`
    - **Control**: `FRAMEfindOrdDest`
    - **Focus Control**: `FRAMEfindOrdDest.DBE`

11. **Invoice Mode**
    - **Caption**: `Invoice Mode`
    - **Control**: `ICBOinvoiceMode`
    - **Type**: ComboBox

12. **Status Info**
    - **Control**: `FRAMEstatusInfo1`
    - **Details**:
      - `DBTXTlastUpd`: Displays last update information.
      - `DBTXTupdBy`: Displays updated by information.
      - `ICBOstat`: Status ComboBox.

### Mapping:
- Not explicitly defined in the code.

---

## 10. Examples and Diagrams:


### Code Snippets:
```delphi
procedure TFORMMconsignee.BTprintCurrentRecordClick(Sender: TObject);
begin
  // Logic to print the current record
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- `FAddressMasterKeyFields`: Used to define the link with addresses, as they are used by multiple entities.
- `FcanDuplicate`: Stores whether the user has permission to duplicate records.

---

## 12. Conclusion:

The `Mconsignee` code unit provides a comprehensive interface for managing consignee-related data. It is modular, with custom frames for specific functionalities, and supports essential actions like duplication and printing. However, the code lacks explicit error handling, field validations, and default values, which could be improved.

---

## 13. Short Summary:

The `Mconsignee` code unit is a form-based interface for managing consignee data, supporting actions like duplication, printing, and detailed data management through tabs. It is modular and extensible but lacks explicit error handling and field validations.#### **Mconsignee.pas**

```
unit Mconsignee;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, ComCtrls, kneFREditSOA, FRconsignee,
  kneFRGridEditSOA, FRconsVehType, FRconsRpPosit,
  FRconsSplDevice, FRconsDelDay, kneFRCtrlEditSOA, DBClient,
  FRconsBook, knePrivileges, sPageControl, sSpeedButton, sBitBtn, ToolWin,
  acCoolBar, sPanel, ImgList, kneEnterAsTab, ActnList, sSplitter,
  FRlistContacts, FRlistAddresses, FRconsMill, FRdocumentsInformation,
  FRconsStandardLogisticCosts, FRextShipDelCons, FRentityComm;

type
  TFORMMconsignee = class(TFORMkneBaseEdit)
    PNLconsignee: TsPanel;
    FRAMEconsignee1: TFRAMEconsignee;
    sSplitter1: TsSplitter;
    PGCdetails: TsPageControl;
    TSHbooking: TsTabSheet;
    FRAMEconsBook1: TFRAMEconsBook;
    TSHweekDelDay: TsTabSheet;
    FRAMEconsDelDay1: TFRAMEconsDelDay;
    TSHallowVehicle: TsTabSheet;
    FRAMEconsVehType1: TFRAMEconsVehType;
    TSHspecialDevice: TsTabSheet;
    FRAMEconsSplDevice1: TFRAMEconsSplDevice;
    TSHunitsPositioning: TsTabSheet;
    FRAMEconsRpPosit1: TFRAMEconsRpPosit;
    SHaddresses: TsTabSheet;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;
    TSHmill: TsTabSheet;
    FRAMEconsMill1: TFRAMEconsMill;
    TSHdocs: TsTabSheet;
    FRAMEdocumentsInformation1: TFRAMEdocumentsInformation;
    PNL1: TsPanel;
    BTNaddress: TsBitBtn;
    ACTduplicate: TAction;
    TSHstdLogisticCost: TsTabSheet;
    FRAMEconsStandardLogisticCosts1: TFRAMEconsStandardLogisticCosts;
    TMRstdLogCost: TTimer;
    TSHextShipDelCons: TsTabSheet;
    FRAMEextShipDelCons1: TFRAMEextShipDelCons;
    TSHconsComm: TsTabSheet;
    FRAMEentityComm1: TFRAMEentityComm;
    procedure FormShow(Sender: TObject);
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FRAMEconsDelDay1BTNaddClick(Sender: TObject);
    procedure ACTduplicateExecute(Sender: TObject);
    procedure BTNewClick(Sender: TObject);
    procedure TMRstdLogCostTimer(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
    procedure PGCdetailsChange(Sender: TObject);
  private
    { Private declarations }
    FAddressMasterKeyFields: String;   // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
    FcanDuplicate: Boolean; //JAR #5354  18-02-2010  Suporte para o Duplicate, guarda se o utilizador tem permiss�o para efectuar o duplicate

    function GetAddrMasterKeyFields: String;
    procedure m_SetContactsDataSet(sender: TObject);
    procedure DoProtectControl(Sender: TObject;
      const pv_Args: array of const);
    function SetAllControlsState(const pv_Control: TControl;
      const pv_Enabled: Boolean): Boolean;
    procedure SetProtectFieldsState(Sender: TFRAMEBaseGridEditSOA);
    procedure SetFormState(pv_AccessMode: string);
    procedure SetFrameState(pv_Frame: TFRAMEBaseGridEditSOA;
      pv_CanEdit: Boolean);                               overload;
    procedure SetFrameState(pv_Frame: TFRAMEBaseCtrlEditSOA;
  pv_CanEdit: Boolean; pv_Fields : array of TClass);       overload;
    function m_ConsInfoIsSimilar(pv_Frame: TFRAMEconsignee): Boolean;

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override;
    procedure m_PutData; override;

  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    constructor Create(AOwner: TComponent);
  published
    property AddressMasterKeyFields: string read GetAddrMasterKeyFields;// � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
  end;

var
  FORMMconsignee: TFORMMconsignee;

implementation

uses  
  kneUtils, Global, kneFRFindEditSOA,
  ConsigneeServiceUtils {NAVOPTECH2022-4055},
  //---
  DRentities;

{$R *.dfm}

{ TFORMMconsignee }

constructor TFORMMconsignee.Create(AOwner: TComponent);
begin
  // As frames do address s�o utilizadas em diversas entidades
  //    (customer, consignee, carrier, agent, warehouse)
  // e em cada uma delas a chave de liga��o do detail (address) com a entidade
  // master � diferente. Para ultrapassar isso a frame Address ao ser criada
  // consulta esta propriedade do form master e inicializa o masterKeyField dela
  // com este valor
  FAddressMasterKeyFields := 'consCode=entityCode;entityType';
  
  inherited;
  PGCdetails.ActivePageIndex := 0;
  FcanDuplicate := ACTduplicate.Enabled;   //JAR #5354 23-02-2010 Apagar este c�digo apos int. o duplicate na kneCBEdit
end;

class function TFORMMconsignee.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
  Result := TFORMMconsignee.Create(Application);
end;

procedure TFORMMconsignee.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  // setup das rela��es master-detail
  FRAMEconsBook1.MasterSource                   := lv_MasterFrame.DStable;
  FRAMEconsDelDay1.MasterSource                 := lv_MasterFrame.DStable;
  FRAMEconsVehType1.MasterSource                := lv_MasterFrame.DStable;
  FRAMEconsSplDevice1.MasterSource              := lv_MasterFrame.DStable;
  FRAMEconsRpPosit1.MasterSource                := lv_MasterFrame.DStable;
  FRAMEdocumentsInformation1.MasterSource       := lv_MasterFrame.DStable;
  FRAMElistAddresses1.MasterSource              := lv_MasterFrame.DStable;    // Detail <- Master
  FRAMElistContacts1.MasterSource               := FRAMElistAddresses1.DStable;   // Detail <- Detail(master)
  FRAMEconsMill1.MasterSource                   := lv_MasterFrame.DStable; // ID2013
  FRAMEconsStandardLogisticCosts1.MasterSource  := lv_MasterFrame.DStable; //JAR #16325:11325  2013-04-23
  FRAMEextShipDelCons1.MasterSource             := lv_MasterFrame.DStable; // [09-02-2015, #21213]
  FRAMEentityComm1.MasterSource                 := lv_MasterFrame.DStable; // [25-03-2019, #23619]

  // parametros standard de servi�os
  lv_MasterFrame.ServiceParams.ShowInactives := True;
//  lv_MasterFrame.ServiceParams.MaxRecords := 0;
//  lv_MasterFrame.ServiceParams.Criteria := '';

  inherited m_getData;
  UseLegacyAccessMode := False;  //JAR #5396 #5397  25-02-2010  Actualiza o estado de edi��o dos controlos em fun��o do accessMode tem de ser ap�s o inherited para ter a lista de frames
  FRAMElistAddresses1.GetDataSet := m_SetContactsDataSet;   // atribui uma referencia para o dadaset dos Contacts
  SetColsWidthInGrid('80;75;90;80;450;100;200;150;100;80', FRAMEdocumentsInformation1.cxDBVtable);  // 07-06-2011, # 8943 novos campos

  if (StringAccessMode = 'NEW') then   //JAR #5354 23-02-2010 Apagar este c�digo apos int. o duplicate na kneCBEdit
    ACTduplicate.Enabled := False
  else
    ACTduplicate.Enabled := FcanDuplicate;

  SetFormState(StringAccessMode);

  if (StringAccessMode = 'VIEW') then   // Este PREGO est� aqui pq no contacts no carregamento dos dados s�o manipulados os conteudos dos campos contactName e contactNameDummy que coloca o dataSet em Edi��o
    Editing := False;

// NAVOPTECH2022-8391  [2024/06/05, alf]
//  //NAVOPTECH2022-5514 -ibsantos -2023-10-04
//  PGCdetails.HandleNeeded;  // evitar erro list index out of bounds associado ao PageControl
//  TSHdocs.TabVisible := SameText(gv_DefaultBusUnit,'TISSUE');
end;

procedure TFORMMconsignee.SetFormState(pv_AccessMode: string);
var
  lv_CanEdit: boolean;
begin
  lv_CanEdit := (pv_accessMode <> 'StdLogCosts') and (pv_accessMode <> 'VIEW');
  SetFrameState(FRAMEconsignee1, lv_CanEdit, []);
  SetFrameState(FRAMEconsBook1, lv_CanEdit, []);
  SetFrameState(FRAMEconsDelDay1, lv_CanEdit);
  SetFrameState(FRAMEconsVehType1, lv_CanEdit);
  SetFrameState(FRAMEconsSplDevice1, lv_CanEdit);
  SetFrameState(FRAMEconsRpPosit1, lv_CanEdit);
  SetFrameState(FRAMEdocumentsInformation1, lv_CanEdit);
  SetFrameState(FRAMElistAddresses1, lv_CanEdit);
  SetFrameState(FRAMElistContacts1, lv_CanEdit);
  SetFrameState(FRAMEconsMill1, lv_CanEdit);
  SetFrameState(FRAMEconsStandardLogisticCosts1,  not lv_CanEdit);
  SetFrameState(FRAMEextShipDelCons1, lv_CanEdit, []);  // [09-02-2015, #21213]
  SetFrameState(FRAMEentityComm1, lv_CanEdit); // [25-03-2019, #23619]
  if not lv_CanEdit then
    BTNaddress.Enabled := False;
end;

procedure TFORMMconsignee.SetFrameState(pv_Frame: TFRAMEBaseGridEditSOA; pv_CanEdit: Boolean);
begin
  with pv_Frame do
  begin
//    cxDBVtable.OptionsData.Inserting := pv_CanEdit;
    cxDBVtable.OptionsData.Editing := pv_CanEdit;
//    cxDBVtable.OptionsData.Deleting := pv_CanEdit;
//    cxDBVtable.OptionsData.Appending := pv_CanEdit;
    if not pv_CanEdit then
      cxDBVtable.OnCellDblClick := nil;
    TkneControls.ProtectAllControls(PNLfooter, [], [], pv_CanEdit);
    PNLfooter.Enabled := True;
    TkneControls.ProtectAllControls(PNLtoolbar, [], [], True);
//    TkneControls.ProtectAllControls(PNLmodify, [], [], pv_CanEdit);
  end;
end;

procedure TFORMMconsignee.SetFrameState(pv_Frame: TFRAMEBaseCtrlEditSOA;
  pv_CanEdit: Boolean; pv_Fields : array of TClass);
begin
  if not pv_CanEdit then
  begin
    with pv_Frame do
    begin
      TkneControls.ProtectAllControls(pv_Frame, [], [], False);
      BTNapply.Enabled := False;
      BTNcancel.Enabled := False;
      BTNadd.Enabled := False;
      BTNdelete.Enabled := False;
      PNLfooter.Enabled := pv_CanEdit;
    end;
  end else
  begin
    if length(pv_Fields) > 0 then
      TkneControls.ProtectAllControls(pv_Frame, pv_Fields, [], False);
  end;
end;


procedure TFORMMconsignee.m_PutData;
var
  lv_ConfResult : Integer;
begin

  // NAVOPTECH2022-4055 [2023/06/15, alf]
  if m_ConsInfoIsSimilar(FRAMEconsignee1) then  // leva Master e Details
  begin
    lv_ConfResult := MessageDlg('There is already a similar record. Do you want to proceed?' ,
      mtConfirmation, [mbYes, mbNo], 0);
    if lv_ConfResult <> mrYes then
      Abort;
  end;

  inherited;

  if (StringAccessMode = 'NEW') then   //JAR #5354 23-02-2010 Apagar este c�digo apos int. o duplicate na kneCBEdit
    ACTduplicate.Enabled := False
  else
    ACTduplicate.Enabled := FcanDuplicate;

end;


procedure TFORMMconsignee.FormShow(Sender: TObject);
begin
  inherited;

  
  if (stringAccessMode = 'StdLogCosts') then
  begin
    // devido a um problema de refresh do raio dos componentes AplhaControls
    // (tabControl) quando o form n�o esta visivel, n�o faz o refresh visual do tab que est� selecionado
    // a solu��o foi ter de colocar um timer :( para for�ar a actualiza��o j�
    // quando o form esta visivel
    TMRstdLogCost.Enabled := True;
  end else
    PGCdetails.ActivePage := TSHmill; // For�a a mostrar a primeira p�gina do Tab Control
//  SetColsWidthInGrid('80;75;90;80;450;100;100;80', FRAMEdocumentsInformation1.cxDBVtable);
end;

procedure TFORMMconsignee.BTprintCurrentRecordClick(Sender: TObject);
begin
  inherited;

  FORMDRentities := nil;
  FORMDRentities := TFORMDRentities.Create(Self);
  FORMDRentities.EDT_code.Text :=
    FRAMEconsignee1.CDStable.FieldByName('consCode').AsString;

  FORMDRentities.FormName := 'consignee.rpt';
  FORMDRentities.ParamReportType := '';

  FORMDRentities.Parent := Self.Parent;
  FORMDRentities.Show;
  FORMDRentities.BTOK.Click;

end;

function TFORMMconsignee.m_Validate: Boolean;
begin
  Result := inherited m_Validate;
end;

// ################ m_SetContactsDataSet  ######################################
procedure TFORMMconsignee.m_SetContactsDataSet(sender: TObject);
begin
  with FRAMElistAddresses1 do
  begin
    CDScontacts := FRAMElistContacts1.CDStable;
  end;
end;

// ################ GetAddrMasterKeyFields  ######################################
function TFORMMconsignee.GetAddrMasterKeyFields: String;
begin
  result := FAddressMasterKeyFields;
end;

procedure TFORMMconsignee.FRAMEconsDelDay1BTNaddClick(Sender: TObject);
begin
  inherited;
  FRAMEconsDelDay1.ACTaddExecute(Sender);

end;


//JAR #5274  18-02-2010
procedure TFORMMconsignee.ACTduplicateExecute(Sender: TObject);
var
  lv_OldKey : string;
begin
  inherited;
  with FRAMEconsignee1 do
  begin
    StringAccessMode := 'NEW';             //23-02-2010 Apagar quando for introduzido o duplicate no kneCBEdit
    ACTduplicate.Enabled := False;

    SetProtectFieldsState(FRAMEconsMill1);
    TkneControls.ProtectAllControls(FRAMElistAddresses1, [], [], True, True, 1, DoProtectControl);
//    FRAMEconsBook1.CHKdelMustBook.Enabled := True;
    SetProtectFieldsState(FRAMEconsDelDay1);
    SetProtectFieldsState(FRAMEconsDelDay1);
    SetProtectFieldsState(FRAMEconsVehType1);
    SetProtectFieldsState(FRAMEconsSplDevice1);
    SetProtectFieldsState(FRAMEconsRpPosit1);
    SetProtectFieldsState(FRAMEdocumentsInformation1);

    SetForEdition(CDStable);

    lv_OldKey := CDStable.FieldByName('consCode').AsString; // [alf, 2023/06/15]

    CDStable.FieldByName('consCode').AsString := 'TEMP';

    // [alf, 2023/06/15] aplicar a mm chave aos restantes detalhes
    fg_SetFieldWithValue(FRAMElistAddresses1.CDStable, 'entityCode', 'TEMP', #39 + lv_OldKey + #39);
    FRAMElistContacts1.m_FillContactName;
    fg_SetFieldWithValue(FRAMElistContacts1.CDStable, 'entityCode', 'TEMP', #39 + lv_OldKey + #39);

    EDTabbrName.SelectAll;
    TkneControls.fg_SetFocus(EDTabbrName);
  end;  
end;



procedure TFORMMconsignee.BTNewClick(Sender: TObject);
begin
  inherited;
  ACTduplicate.Enabled := False;

end;

procedure TFORMMconsignee.SetProtectFieldsState(Sender: TFRAMEBaseGridEditSOA);
var
  lv_ROFields: TStringList;
  i: Integer;
  lv_CanEdit: Boolean;
begin
  lv_ROFields := nil;
  
  try
    // protec��o de edi��o para os campos Read-Only
    lv_ROFields := TStringList.Create;
    lv_ROFields.Delimiter := ';';
    lv_ROFields.DelimitedText := kneUtils.TkneDB.GetReadOnlyFields(Sender.CDStable);

    lv_CanEdit := True;
    if (Sender.AccessMode = 'VIEW') or (Sender.AccessMode= 'DELETE') then 
      lv_CanEdit := False;

    Sender.cxDBVtable.OptionsData.Editing := lv_CanEdit;
    TkneControls.ProtectAllControls(Sender, [], [], lv_CanEdit, True, 1, DoProtectControl);  //UseLegacyMode 1 : para for�ar que tb os paineis e frames sejam afectados pela altera��o de estado (Enable/Disable)
    kneUtils.TkneGrid.fg_SetFieldsState(Sender.cxDBVtable, lv_ROFields, False, True);     //JAR #5402  2/25/2010
  finally
    if Assigned(lv_ROFields) then FreeAndNil(lv_ROFields);
  end;
end;

procedure TFORMMconsignee.DoProtectControl(Sender: TObject;
  const pv_Args: array of const);
var
  lv_ValidArgs: Boolean;
begin
  // check args
  lv_ValidArgs := (Length(pv_Args) = 1) and (pv_Args[0].VType = vtBoolean);

  if not lv_ValidArgs then
    raise Exception.Create('Protect Control: Invalid arguments for method.');

  SetAllControlsState(TControl(Sender), pv_Args[0].VBoolean);
end;


function TFORMMconsignee.SetAllControlsState(
  const pv_Control: TControl; const pv_Enabled: Boolean): Boolean;
begin
  if not Assigned(pv_Control) then Exit;

//   Find Edit
  if pv_Control.InheritsFrom(TFRAMEFindEditSOA) then
  begin
    TFRAMEFindEditSOA(pv_Control).Enable := pv_Enabled;
    Exit;
  end; 

  pv_Control.Enabled := pv_Enabled;

  // Forms, Frames, Datasets, labels - deve ignorar
  if TkneTypeInfo.fg_ClassInheritsFrom(pv_Control.ClassType, 'TForm')
      or TkneTypeInfo.fg_ClassInheritsFrom(pv_Control.ClassType, 'TFrame')
      or TkneTypeInfo.fg_ClassInheritsFrom(pv_Control.ClassType, 'TDataSet')
      or TkneTypeInfo.fg_ClassInheritsFrom(pv_Control.ClassType, 'TLabel')
      or TkneTypeInfo.fg_ClassInheritsFrom(pv_Control.ClassType, 'TPanel') then
    Exit;

  if TkneTypeInfo.fg_HasProperty(pv_Control, 'TabStop') then
    TkneTypeInfo.fg_SetBooleanPropertyValue(pv_Control, 'TabStop', pv_Enabled);
end;

// devido a um problema de refresh do raio dos componentes AplhaControls
// (tabControl) quando o form n�o esta visivel, n�o faz o refresh visual do tab que est� selecionado
// a solu��o foi ter de colocar um timer :( para for�ar a actualiza��o j�
// quando o form esta visivel
procedure TFORMMconsignee.TMRstdLogCostTimer(Sender: TObject);
begin
  PGCdetails.ActivePageIndex := 8;
  Application.ProcessMessages;
  PGCdetails.Repaint;
  TSHstdLogisticCost.Repaint;
end;

procedure TFORMMconsignee.BTCancelClick(Sender: TObject);
begin
  inherited;
  FRAMEconsStandardLogisticCosts1.m_SetLoadsBtnStat; // [02-11-2016, #22894]
end;

// [25-03-2019, #23619]
procedure TFORMMconsignee.PGCdetailsChange(Sender: TObject);
begin
  inherited;
  if StringAccessMode <> 'VIEW' then
    if PGCdetails.ActivePage = TSHconsComm then
      FRAMEentityComm1.BusUnitValuesForCbx := FRAMEconsMill1.SelectedBusUnits;
end;

// NAVOPTECH2022-4055 [2023/06/15, alf]
function TFORMMconsignee.m_ConsInfoIsSimilar(pv_Frame: TFRAMEconsignee): Boolean;
var
  lv_Service : TConsigneeServiceUtils;
  lv_DataPacketList, lv_PropertyNameList : TStrings;
  lv_DataSetList: TList;
begin
  Result := False;

  lv_DataSetList := nil;
  lv_DataPacketList := nil;
  lv_PropertyNameList := nil;

  Screen.Cursor := crHourGlass;

  lv_Service := TConsigneeServiceUtils(FRAMEconsignee1.ProviderService);

  lv_DataPacketList   := TStringList.Create;
  lv_PropertyNameList := TStringList.Create;
  lv_DataSetList      := TList.Create;

  try
    lv_DataSetList.Add(FRAMEconsignee1.CDStable);
    lv_DataSetList.Add(FRAMElistAddresses1.CDStable);

    lv_DataPacketList.Delimiter := ';';
    lv_DataPacketList.DelimitedText := 'Consignee;Address';

    lv_PropertyNameList.Delimiter := ';';
    lv_PropertyNameList.DelimitedText := ';addresses';

    Result := lv_Service.ExistSimilarConsignee(
                              lv_DataSetList,
                              lv_DataPacketList,
                              lv_PropertyNameList);

  finally
    if Assigned(lv_DataPacketList) then FreeAndNil(lv_DataPacketList);
    if Assigned(lv_DataSetList) then FreeAndNil(lv_DataSetList);
    if Assigned(lv_PropertyNameList) then FreeAndNil(lv_PropertyNameList);
  end;
 
end;

end.

```

#### **Mconsignee.dfm**

```
inherited FORMMconsignee: TFORMMconsignee
  Left = 466
  Top = 87
  Width = 975
  Height = 788
  Caption = 'Consignee Maintenance'
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLtoolbar: TsPanel
    Width = 959
    inherited CLBactions: TsCoolBar
      Width = 959
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 955
        end>
      inherited PNbotoes: TsPanel
        Width = 942
        inherited BTNseparator2: TsSpeedButton
          Left = 640
        end
        inherited PNLprint: TsPanel
          Visible = True
          inherited BTprintCurrentRecord: TsBitBtn
            OnClick = BTprintCurrentRecordClick
          end
        end
        object PNL1: TsPanel
          Left = 552
          Top = 1
          Width = 88
          Height = 39
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 7
          SkinData.SkinSection = 'ALPHACOMBOBOX'
          object BTNaddress: TsBitBtn
            Left = 3
            Top = 2
            Width = 82
            Height = 30
            Action = ACTduplicate
            Caption = 'Duplicate'
            TabOrder = 0
            TabStop = False
            SkinData.SkinSection = 'SPEEDBUTTON'
            ImageIndex = 10
            Images = IMLbuttons
          end
        end
      end
    end
  end
  object PNLconsignee: TsPanel [2]
    Left = 0
    Top = 41
    Width = 959
    Height = 709
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    object sSplitter1: TsSplitter
      Left = 1
      Top = 312
      Width = 957
      Height = 4
      Cursor = crVSplit
      Align = alTop
      SkinData.SkinSection = 'FORM'
    end
    inline FRAMEconsignee1: TFRAMEconsignee
      Left = 1
      Top = 1
      Width = 957
      Height = 311
      Align = alTop
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited LBLcountry: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindCountry.DBE
      end
      inherited LBLstate: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindOrdDest.DBE
      end
      inherited LBLmarket: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindConsMarket.DBE
      end
      inherited LBLwarehouse: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindWarehouse.DBE
      end
      inherited Label2: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindDelTerm.DBE
      end
      inherited LBLdelPolicy: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindDelPolicy.DBE
      end
      inherited LBLsysMesure: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindUnitSys.DBE
      end
      inherited LBLdestination: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindDestination.DBE
      end
      inherited Label1: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindLanguage.DBE
      end
      inherited Label6: TsLabel
        FocusControl = FRAMEconsignee1.FRAMEfindOrdDest.DBE
      end
      inherited PNLfooter: TsPanel
        Top = 277
        Width = 957
      end
      inherited FRAMEfindConsMarket: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 209
          end
        end
      end
      inherited FRAMEfindDestination: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 209
          end
        end
      end
      inherited FRAMEfindDelPolicy: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 209
          end
        end
      end
      inherited ICBOinvoiceMode: TcxDBImageComboBox
        Width = 89
      end
      inherited FRAMEstatusInfo1: TFRAMEstatusInfo
        Top = 232
        Width = 957
        inherited GRPstatus: TsGroupBox
          Width = 957
          inherited DBTXTlastUpd: TsDBText
            DataSource = FRAMEconsignee1.DStable
          end
          inherited DBTXTupdBy: TsDBText
            DataSource = FRAMEconsignee1.DStable
          end
          inherited ICBOstat: TcxDBImageComboBox
            Properties.Images = FRAMEconsignee1.IMLeditActions
            Width = 97
          end
        end
      end
      inherited FRAMEfindCountry: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 233
          end
        end
      end
      inherited FRAMEfindState: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 233
          end
        end
      end
      inherited FRAMEfindWarehouse: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 233
          end
        end
      end
      inherited FRAMEfindDelTerm: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 233
          end
        end
      end
      inherited FRAMEfindUnitSys: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 233
          end
        end
      end
      inherited FRAMEfindLanguage: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 0
          end
        end
      end
    end
    object PGCdetails: TsPageControl
      Left = 1
      Top = 316
      Width = 957
      Height = 392
      ActivePage = TSHmill
      Align = alClient
      TabOrder = 1
      OnChange = PGCdetailsChange
      SkinData.SkinSection = 'PAGECONTROL'
      object TSHmill: TsTabSheet
        Caption = 'Mill'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEconsMill1: TFRAMEconsMill
          Left = 0
          Top = 0
          Width = 949
          Height = 364
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 949
            Height = 330
          end
          inherited PNLfooter: TsPanel
            Top = 330
            Width = 949
          end
        end
      end
      object TSHbooking: TsTabSheet
        Caption = 'B&ooking '
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEconsBook1: TFRAMEconsBook
          Left = 0
          Top = 0
          Width = 949
          Height = 364
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited PNLfooter: TsPanel
            Top = 330
            Width = 949
          end
          inherited ICBOwhoBook: TcxDBImageComboBox
            Width = 105
          end
          inherited FRAMEstatusInfo1: TFRAMEstatusInfo
            inherited GRPstatus: TsGroupBox
              inherited DBTXTlastUpd: TsDBText
                DataSource = FRAMEconsBook1.DStable
              end
              inherited DBTXTupdBy: TsDBText
                DataSource = FRAMEconsBook1.DStable
              end
              inherited ICBOstat: TcxDBImageComboBox
                Width = 97
              end
            end
          end
        end
      end
      object TSHweekDelDay: TsTabSheet
        Caption = '&Week Delivery Days'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEconsDelDay1: TFRAMEconsDelDay
          Left = 0
          Top = 0
          Width = 949
          Height = 364
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 949
            Height = 330
          end
          inherited PNLfooter: TsPanel
            Top = 330
            Width = 949
          end
        end
      end
      object TSHallowVehicle: TsTabSheet
        Caption = 'Allowed &Vehicle'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEconsVehType1: TFRAMEconsVehType
          Left = 0
          Top = 0
          Width = 949
          Height = 364
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 949
            Height = 330
          end
          inherited PNLfooter: TsPanel
            Top = 330
            Width = 949
          end
        end
      end
      object TSHspecialDevice: TsTabSheet
        Caption = 'S&pecial Device'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEconsSplDevice1: TFRAMEconsSplDevice
          Left = 0
          Top = 0
          Width = 949
          Height = 364
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 949
            Height = 330
          end
          inherited PNLfooter: TsPanel
            Top = 330
            Width = 949
          end
        end
      end
      object TSHunitsPositioning: TsTabSheet
        Caption = '&Units Positioning'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEconsRpPosit1: TFRAMEconsRpPosit
          Left = 0
          Top = 0
          Width = 949
          Height = 364
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 949
            Height = 330
          end
          inherited PNLfooter: TsPanel
            Top = 330
            Width = 949
          end
        end
      end
      object SHaddresses: TsTabSheet
        Caption = '&Addresses'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object SPL2: TsSplitter
          Left = 0
          Top = 100
          Width = 949
          Height = 4
          Cursor = crVSplit
          Align = alTop
          SkinData.SkinSection = 'FORM'
        end
        inline FRAMElistAddresses1: TFRAMElistAddresses
          Left = 0
          Top = 0
          Width = 949
          Height = 100
          Align = alTop
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 949
            Height = 66
          end
          inherited PNLfooter: TsPanel
            Top = 66
            Width = 949
          end
        end
        inline FRAMElistContacts1: TFRAMElistContacts
          Left = 0
          Top = 104
          Width = 949
          Height = 260
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 1
          inherited cxDBG: TcxGrid
            Width = 949
            Height = 226
          end
          inherited PNLfooter: TsPanel
            Top = 226
            Width = 949
          end
        end
      end
      object TSHdocs: TsTabSheet
        Caption = 'Documentation'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEdocumentsInformation1: TFRAMEdocumentsInformation
          Left = 0
          Top = 0
          Width = 949
          Height = 364
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 949
            Height = 330
          end
          inherited PNLfooter: TsPanel
            Top = 330
            Width = 949
          end
        end
      end
      object TSHstdLogisticCost: TsTabSheet
        Caption = 'Standard Logistic Costs'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEconsStandardLogisticCosts1: TFRAMEconsStandardLogisticCosts
          Left = 0
          Top = 0
          Width = 949
          Height = 364
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 949
            Height = 330
          end
          inherited PNLfooter: TsPanel
            Top = 330
            Width = 949
          end
        end
      end
      object TSHextShipDelCons: TsTabSheet
        Caption = 'PreDelivery Confirmation'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEextShipDelCons1: TFRAMEextShipDelCons
          Left = 0
          Top = 0
          Width = 949
          Height = 364
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited PNLfooter: TsPanel
            Top = 330
            Width = 949
          end
          inherited EDTalertTime: TcxDBTimeEdit
            Width = 65
          end
          inherited EDTalertDays: TcxDBMaskEdit
            Width = 33
          end
        end
      end
      object TSHconsComm: TsTabSheet
        Caption = 'Comments'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEentityComm1: TFRAMEentityComm
          Left = 0
          Top = 0
          Width = 949
          Height = 364
          Align = alClient
          ParentBackground = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 949
            Height = 330
          end
          inherited PNLfooter: TsPanel
            Top = 330
            Width = 949
          end
        end
      end
    end
  end
  inherited ACLcustomButtons: TActionList
    Left = 725
    object ACTduplicate: TAction
      Tag = 10
      Caption = 'Duplicate'
      OnExecute = ACTduplicateExecute
    end
  end
  object TMRstdLogCost: TTimer
    Enabled = False
    Interval = 300
    OnTimer = TMRstdLogCostTimer
    Left = 488
    Top = 73
  end
end

```
<!-- tabs:end -->

