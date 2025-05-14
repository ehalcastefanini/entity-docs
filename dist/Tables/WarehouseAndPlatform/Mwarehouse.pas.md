<!-- tabs:start -->

#### **Documentation**

# Documentation for `Mwarehouse` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `Mwarehouse` code unit is designed to manage and edit warehouse-related data, including carriers, services, destinations, addresses, and mills. It provides a user interface for interacting with warehouse data and ensures proper linkage between master and detail entities, such as addresses and warehouses. The main objective is to streamline the management of warehouse information in a structured and user-friendly manner.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its object-oriented programming features.
- **VCL Components**: Includes components like `TPanel`, `TsPanel`, `TsSplitter`, `TsPageControl`, and custom frames for specific functionalities.
- **Database Interaction**: Uses `TDataSet` and related components for database operations.
- **Custom Components**: Includes custom frames like `TFRAMEwarehouse`, `TFRAMEwarehouseCarrier`, `TFRAMEwarehouseServices`, etc., for modular functionality.

### Form Type:
This code represents a **form** with the following elements:
- **Form Elements and Types**:
  - `PNLDescription`: Panel for displaying descriptions.
  - `PNLwarehouse`: Main panel for warehouse details.
  - `PGCdetails`: Page control with tabs for different sections (Carriers, Services, Destinations, Addresses, Mills).
  - `FRAMEwarehouse1`: Custom frame for warehouse details.
  - `FRAMEwarehouseCarrier1`: Custom frame for carrier details.
  - `FRAMEwarehouseServices1`: Custom frame for services.
  - `FRAMEwarehouseDest1`: Custom frame for destinations.
  - `FRAMElistAddresses1`: Custom frame for addresses.
  - `FRAMElistContacts1`: Custom frame for contacts.
  - `FRAMEwarehouseMills1`: Custom frame for mills.
- **Form Actions and Effects**:
  - `FormShow`: Initializes the form and loads data.
  - `BTprintCurrentRecordClick`: Prints the current record.
  - `FRAMEwarehouse1EDTwhseCodeEnter` and `FRAMEwarehouse1EDTwhseCodeExit`: Handle events when entering or exiting the warehouse code field.
  - `FRAMEwarehouseMills1CDStableBeforePost`: Validates data before posting changes to the database.

---

## 2. Functionality Description:

### User/Software Actions:
- View and edit warehouse details.
- Navigate through different tabs to manage carriers, services, destinations, addresses, and mills.
- Print the current warehouse record.
- Validate and save data changes.

### Main Components:
- **Custom Frames**: Modular components for specific sections (e.g., `TFRAMEwarehouse`, `TFRAMEwarehouseCarrier`).
- **Page Control (`PGCdetails`)**: Organizes the interface into tabs for better navigation.
- **Database Interaction**: Handles data retrieval, validation, and saving.

### Pseudo-code for Actions and Events:
- `On FormShow`: `if form is shown then initialize data and load warehouse details`.
- `On Button Click (Print)`: `if print button clicked then print current record`.
- `On Field Enter`: `if warehouse code field is entered then perform specific actions`.
- `On Field Exit`: `if warehouse code field is exited then validate field`.
- `On Before Post`: `if data is about to be posted then validate data`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created and initialized (`Create` constructor).
   - The `FormShow` event is triggered to load data and set up the interface.
2. **User Interaction**:
   - Users navigate through tabs to view/edit details.
   - Specific actions (e.g., printing, saving) are triggered by button clicks or field events.
3. **Data Handling**:
   - Data is validated before being saved to the database.

### Functions and File Locations:
- `FormShow` (in `Mwarehouse`): Initializes the form and loads data.
- `BTprintCurrentRecordClick` (in `Mwarehouse`): Handles printing of the current record.
- `m_getData` (in `Mwarehouse`): Retrieves data for the form.
- `m_Validate` (in `Mwarehouse`): Validates data before saving.

### Required User Data:
- Warehouse details (e.g., code, description).
- Carrier, service, destination, address, and mill information.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Print Button**: Enabled only if a record is selected.
- **Save Data**: Allowed only if all required fields are valid.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Required field not completed" if a required field is empty.
- "Invalid data" if validation fails.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Warehouse code: Must be unique and non-empty.
- Address fields: Must be linked to the correct master entity.

---

## 5. Main Functions:

- **`m_CreateFormEdit`**: Creates and initializes the form.
- **`m_getData`**: Retrieves data for the form and its components.
- **`m_Validate`**: Validates data before saving.
- **`BTprintCurrentRecordClick`**: Prints the current record.

---

## 6. API Service Consumption:

No external API calls are defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- The "Address" fields are linked dynamically based on the master entity (`warehouseCode=entityCode;entityType`).

---

## 8. Dependencies:

### External Libraries:
- **VCL Components**: Used for UI elements.
- **Custom Components**: Includes `TFRAMEwarehouse`, `TFRAMEwarehouseCarrier`, etc.

### Custom Components:
- `TFRAMEwarehouse`: Manages warehouse details.
- `TFRAMEwarehouseCarrier`: Manages carrier details.
- `TFRAMEwarehouseServices`: Manages service details.
- `TFRAMEwarehouseDest`: Manages destination details.
- `TFRAMElistAddresses`: Manages address details.
- `TFRAMElistContacts`: Manages contact details.
- `TFRAMEwarehouseMills`: Manages mill details.

---

## 9. Fields and Validations Listing:

- **Warehouse Code**: (type: string, required, unique).
- **Description**: (type: string, optional).
- **Carriers, Services, Destinations, Addresses, Mills**: Managed through respective frames.

Field constraints and validations are not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFORMMwarehouse.FormShow(Sender: TObject);
begin
  m_getData;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Address Master Key Fields**: Defines the linkage between addresses and master entities.
- **Optimization**: Uses `TFRAMEBaseEditSOA` for resource optimization.

---

## 12. Conclusion:

The `Mwarehouse` code unit provides a robust framework for managing warehouse-related data. Its modular design and use of custom frames make it extensible and maintainable. However, the lack of explicit field validations and error handling could be improved.

---

## 13. Short Summary:

The `Mwarehouse` code unit manages warehouse data, including carriers, services, destinations, addresses, and mills. It uses a modular design with custom frames and ensures proper linkage between master and detail entities.#### **Mwarehouse.pas**

```
unit Mwarehouse;

interface
                                      
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, kneFREditSOA, DBClient,
  kneFRGridEditSOA, FRwarehouse, ComCtrls, FRwarehouseCarrier,
  FRwarehouseServices, kneFRCtrlEditSOA, FRwarehouseDest, knePrivileges,
  sPageControl, sSpeedButton, sBitBtn, ToolWin, acCoolBar, sPanel, ImgList,
  kneEnterAsTab, ActnList, sSplitter, FRlistContacts, FRlistAddresses,
  FRwarehouseMills, DB;

type
  TFORMMwarehouse = class(TFORMkneBaseEdit)
    PNLDescription: TPanel;
    PNLwarehouse: TsPanel;
    FRAMEwarehouse1: TFRAMEwarehouse;
    sSplitter1: TsSplitter;
    PGCdetails: TsPageControl;
    TSHcarriers: TsTabSheet;
    FRAMEwarehouseCarrier1: TFRAMEwarehouseCarrier;
    TSHservices: TsTabSheet;
    FRAMEwarehouseServices1: TFRAMEwarehouseServices;
    TSHdestination: TsTabSheet;
    FRAMEwarehouseDest1: TFRAMEwarehouseDest;
    TSHaddresses: TsTabSheet;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;
    TSHmills: TsTabSheet;
    FRAMEwarehouseMills1: TFRAMEwarehouseMills;
    procedure FormShow(Sender: TObject);
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FRAMEwarehouse1EDTwhseCodeEnter(Sender: TObject);
    procedure FRAMEwarehouse1EDTwhseCodeExit(Sender: TObject);
    procedure FRAMEwarehouseMills1CDStableBeforePost(DataSet: TDataSet);
  private
    { Private declarations }
    FAddressMasterKeyFields: String;   // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
    FoldWhse: string;                  //JAR #5863  14-05-2010
    function GetAddrMasterKeyFields: String;
    procedure m_SetContactsDataSet(sender: TObject);

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override;

  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    constructor Create(AOwner: TComponent);

  published
    property AddressMasterKeyFields: string read GetAddrMasterKeyFields;// � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
  end;

var
  FORMMwarehouse: TFORMMwarehouse;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes,
  //---
  DRentities;

{ TFORMMwarehouse }


constructor TFORMMwarehouse.Create(AOwner: TComponent);
begin
  // As frames do address s�o utilizadas em diversas entidades
  //    (customer, consignee, carrier, agent, warehouse)
  // e em cada uma delas a chave de liga��o do detail (address) com a entidade
  // master � diferente. Para ultrapassar isso a frame Address ao ser criada
  // consulta esta propriedade do form master e inicializa o masterKeyField dela
  // com este valor
  FAddressMasterKeyFields := 'warehouseCode=entityCode;entityType';

  inherited;
end;

class function TFORMMwarehouse.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
  Result := TFORMMwarehouse.Create(Application);
end;

procedure TFORMMwarehouse.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  FRAMEwarehouseCarrier1.MasterSource := lv_MasterFrame.DStable;
```

#### **Mwarehouse.dfm**

```
inherited FORMMwarehouse: TFORMMwarehouse
  Left = 420
  Top = 185
  Height = 535
  Caption = 'Warehouses and Platforms'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object PNLDescription: TPanel [1]
    Left = 512
    Top = 304
    Width = 277
    Height = 197
    BevelOuter = bvNone
    TabOrder = 1
  end
  inherited PNLtoolbar: TsPanel
    Width = 789
    inherited CLBactions: TsCoolBar
      Width = 789
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 785
        end>
      inherited PNbotoes: TsPanel
        Width = 772
      end
    end
  end
  object PNLwarehouse: TsPanel [3]
    Left = 0
    Top = 41
    Width = 789
    Height = 460
    Align = alClient
    TabOrder = 2
    SkinData.SkinSection = 'PANEL'
    object sSplitter1: TsSplitter
      Left = 1
      Top = 208
      Width = 787
      Height = 4
      Cursor = crVSplit
      Align = alTop
      SkinData.SkinSection = 'FORM'
    end
    inline FRAMEwarehouse1: TFRAMEwarehouse
      Left = 1
      Top = 1
      Width = 787
      Height = 207
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited LBLcountry: TsLabel
        FocusControl = FRAMEwarehouse1.FRAMEfindCountry.DBE
      end
      inherited Label1: TsLabel
        FocusControl = FRAMEwarehouse1.FRAMEfindLanguage.DBE
      end
      inherited Label2: TsLabel
        FocusControl = FRAMEwarehouse1.FRAMEfindConsignee.DBE
      end
      inherited Label3: TsLabel
        FocusControl = FRAMEwarehouse1.FRAMEfindCarrier.DBE
      end
      inherited PNLfooter: TsPanel
        Top = 173
        Width = 787
      end
      inherited FRAMEstatusInfo1: TFRAMEstatusInfo
        Top = 128
        Width = 787
        inherited GRPstatus: TsGroupBox
          Width = 787
          inherited DBTXTlastUpd: TsDBText
            DataSource = FRAMEwarehouse1.DStable
          end
          inherited DBTXTupdBy: TsDBText
            DataSource = FRAMEwarehouse1.DStable
          end
          inherited ICBOstat: TcxDBImageComboBox
            Width = 97
          end
        end
      end
      inherited FRAMEfindCountry: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            Width = 236
          end
```
<!-- tabs:end -->

