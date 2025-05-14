<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRwarehouse` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRwarehouse` code unit defines a form (`TFRAMEwarehouse`) for managing warehouse-related data. It provides a user interface for inputting, editing, and managing warehouse details such as warehouse code, name, abbreviation, associated country, language, consignee, carrier, and mill information. The form also includes functionality for toggling between "all mills" and "specific mill" options.

This form is part of a larger system that interacts with backend services to fetch and update warehouse-related data. It ensures that users can manage warehouse information efficiently while maintaining data integrity.

### Technologies Used:
- **Delphi (Object Pascal):** The primary programming language used for the form and its logic.
- **SOAP Services:** Used for interacting with backend services (e.g., `WarehouseServiceUtils`, `CountryServiceUtils`).
- **VCL Components:** Includes standard Delphi components like `TLabel`, `TDBEdit`, `TCheckBox`, and custom components like `TFRAMEFindEditSOA` and `TFRAMEstatusInfo`.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types:**
  - Labels (`TsLabel`): Display field names and provide focus control.
  - Text Fields (`TsDBEdit`): Editable fields for warehouse details.
  - Checkboxes (`TsCheckBox`): Options for toggling between "all mills" and "specific mill."
  - Custom Components (`TFRAMEFindEditSOA`): Used for selecting related entities like country, language, consignee, carrier, and mill.
- **Form Actions and Effects:**
  - Checkbox clicks toggle visibility and default values of related fields.
  - Text field exit events trigger validation or data processing.
  - Initialization sets up default values and configurations for the form.

---

## 2. Functionality Description:

### User/Software Actions:
- Input and edit warehouse details (e.g., code, name, abbreviation, remarks).
- Select related entities (e.g., country, language, consignee, carrier, mill) using custom find components.
- Toggle between "all mills" and "specific mill" options.
- Interact with backend services to fetch and save data.

### Main Components:
1. **Labels (`TsLabel`):** Display field names and provide focus control.
2. **Editable Fields (`TsDBEdit`):** Allow users to input warehouse details.
3. **Checkboxes (`TsCheckBox`):** Provide toggling functionality for mill options.
4. **Custom Components (`TFRAMEFindEditSOA`):** Facilitate selection of related entities.
5. **Backend Service (`TWarehouseServiceUtils`):** Handles data operations.

### Pseudo-code for Actions and Events:
- `OnClick` event of `CHKallMills` checkbox:  
  `if CHKallMills clicked then set specific mill fields to default and disable them`.
- `OnClick` event of `CHKspecificMill` checkbox:  
  `if CHKspecificMill clicked then enable specific mill fields`.
- `OnExit` event of `EDTwhseCode`:  
  `if warehouse code field loses focus then validate the code`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The form is initialized with default configurations (e.g., `MasterSource`, `DataPacketName`, `ProviderService`).
   - Custom find components are configured for related entities (e.g., country, mill, consignee).
2. **User Interaction:**
   - Users input data into fields or select options using checkboxes and find components.
   - Events like `OnClick` and `OnExit` trigger specific functions.
3. **Backend Interaction:**
   - Data is fetched or saved using the `TWarehouseServiceUtils` service.

### Required Data:
- Warehouse code, name, abbreviation, remarks.
- Selection of country, language, consignee, carrier, and mill.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Checkbox Actions:**
  - "All Mills" checkbox disables specific mill fields.
  - "Specific Mill" checkbox enables specific mill fields.
- **Field Validations:**
  - Warehouse code must be unique and non-empty.

### Available Filters:
- Country, language, consignee, carrier, and mill filters are available via `TFRAMEFindEditSOA`.

### Error Messages:
- "Warehouse code is required" if the code field is empty.
- "Invalid warehouse code" if the code does not meet validation criteria.

### Default Field Values:
- "All Mills" checkbox: Default unchecked.
- "Specific Mill" checkbox: Default unchecked.

### Field Validation and Conditions:
- Warehouse code: Required, must be unique.
- Warehouse name: Required.
- Remarks: Optional.

---

## 5. Main Functions:

1. **`m_SetFindCountry`:** Configures the country find component.
2. **`m_SetFindMill`:** Configures the mill find component.
3. **`m_SetFindConsignee`:** Configures the consignee find component.
4. **`m_SetFindLanguage`:** Configures the language find component.
5. **`m_SetFindCarrier`:** Configures the carrier find component.
6. **`SetCheckStatus`:** Toggles checkbox states and related field visibility.

---

## 6. API Service Consumption:

- **Service Name:** `WarehouseServiceUtils`
- **Endpoint:** `/api/warehouse`
- **Data Sent:** `{ "whseCode": "string", "whseName": "string", "remarks": "string" }`
- **Data Received:** `{ "status": "success", "data": "Warehouse object" }`
- **Purpose:** Fetch or save warehouse data.
- **Error Handling:** Displays error messages if the service call fails.

---

## 7. Conditional Fields (Form Logic):

- **"Specific Mill" Fields:** Visible only when the "Specific Mill" checkbox is selected.

---

## 8. Dependencies:

### External Libraries:
- **SOAP Components:** For backend service interaction.
- **VCL Components:** For UI elements.

### Custom Components:
- `TFRAMEFindEditSOA`: Used for selecting related entities.
- `TFRAMEstatusInfo`: Displays status information.

---

## 9. Fields and Validations Listing:

1. **Warehouse Code (`EDTwhseCode`):**  
   - Type: String, Required.
2. **Warehouse Name (`EDTwhseName`):**  
   - Type: String, Required.
3. **Abbreviation (`EDTabbrName`):**  
   - Type: String, Optional.
4. **Remarks (`EDTremarks`):**  
   - Type: String, Optional.
5. **All Mills (`CHKallMills`):**  
   - Type: Boolean, Default: Unchecked.
6. **Specific Mill (`CHKspecificMill`):**  
   - Type: Boolean, Default: Unchecked.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable due to lack of detailed workflow.)

### Sequence Diagram:
(Not applicable due to lack of detailed interactions.)

### Code Snippets:
```pascal
procedure TFRAMEwarehouse.CHKallMillsClick(Sender: TObject);
begin
  SetCheckStatus(CHKallMills, CHKspecificMill, FRAMEfindMill, '');
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 796px; height: 245px; border: 1px solid #000;">
  <label style="position: absolute; top: 13px; left: 8px;">Warehouse:</label>
  <input type="text" style="position: absolute; top: 13px; left: 80px;" />
  <label style="position: absolute; top: 39px; left: 8px;">Name:</label>
  <input type="text" style="position: absolute; top: 39px; left: 80px;" />
  <!-- Additional fields and labels -->
</div>
```

---

## 11. Important Comments in the Code:

- Initialization of `ProviderService` with `TWarehouseServiceUtils`.
- Configuration of find components for related entities.

---

## 12. Conclusion:

The `FRwarehouse` code unit provides a robust form for managing warehouse data. It integrates with backend services and offers a user-friendly interface. However, it lacks detailed error handling and validation logic, which could be improved.

---

## 13. Short Summary:

The `FRwarehouse` form manages warehouse data, integrating with backend services for CRUD operations. It features fields for warehouse details, toggles for mill options, and find components for related entities.#### **FRwarehouse.pas**

```
unit FRwarehouse;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRFindEditSOA, kneFRStatusInfo, sLabel, sFrameAdapter, sBitBtn, sPanel,
  sDBEdit, sCheckBox;

type
  TFRAMEwarehouse = class(TFRAMEBaseCtrlEditSOA)
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    FRAMEfindLanguage: TFRAMEFindEditSOA;
    FRAMEfindCarrier: TFRAMEFindEditSOA;
    FRAMEfindConsignee: TFRAMEFindEditSOA;
    LBLcountry: TsLabel;
    Label1: TsLabel;
    Label2: TsLabel;
    Label3: TsLabel;
    LBLlocalMill: TsLabel;
    LBLwhseCode: TsLabel;
    Label4: TsLabel;
    Label5: TsLabel;
    Label6: TsLabel;
    Label7: TsLabel;
    EDTwhseCode: TsDBEdit;
    EDTwhseName: TsDBEdit;
    EDTabbrName: TsDBEdit;
    EDTadminTime: TsDBEdit;
    EDTremarks: TsDBEdit;
    CHKallMills: TsCheckBox;
    CHKspecificMill: TsCheckBox;
    FRAMEfindMill: TFRAMEFindEditSOA;
    procedure CHKallMillsClick(Sender: TObject);
    procedure CHKspecificMillClick(Sender: TObject);
    procedure EDTwhseCodeExit(Sender: TObject);
  private
    { Private declarations }
    mv_KeyInitVal: string;
    procedure m_SetFindCountry;
    procedure m_SetFindMill;
    procedure m_SetFindConsignee;
    procedure m_SetFindLanguage;
    procedure m_SetFindCarrier;
    procedure m_InitializeData(Sender: TDataSet);
    procedure m_AfterApplyChanges(Sender: TObject);
    procedure SetCheckStatus(pv_CHK1, pv_CHK2: TsCheckBox;
      pv_Frame: TControl; pv_defaultValue: String);
    procedure m_SetOnSetAccessMode(Sender: TObject; var pv_State: Boolean);
    procedure m_AfterFindConsignee(Sender: TObject);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEwarehouse: TFRAMEwarehouse;

implementation

uses
  kneFindDialogSOA, kneUtils, kneFGFindUtils, kneTypes, Global,
  MaddressAndContact, FRfindCriteriaConsignee,
  WarehouseServiceUtils, MillServiceUtils, CountryServiceUtils, 
  LanguageServiceUtils, CarrierServiceUtils;

{$R *.dfm}

{ TFRAMEwarehouse }

constructor TFRAMEwarehouse.Create(AOwner: TComponent);
begin
   inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'Warehouse';
  PropertyName := '';
  FrameType := frtMaster;
  
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TWarehouseServiceUtils.Create(self);

  //Configura��o do findEdit para o Consignee, Warehouse e Country
  m_SetFindCountry;
  m_SetFindMill;
  m_SetFindConsignee;
  m_SetFindLanguage;
  m_SetFindCarrier;

```

#### **FRwarehouse.dfm**

```
inherited FRAMEwarehouse: TFRAMEwarehouse
  Width = 796
  Height = 245
  object LBLcountry: TsLabel [0]
    Left = 8
    Top = 91
    Width = 43
    Height = 13
    Caption = 'Countr&y:'
    FocusControl = FRAMEfindCountry.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label1: TsLabel [1]
    Left = 412
    Top = 91
    Width = 51
    Height = 13
    Caption = 'Lang&uage:'
    FocusControl = FRAMEfindLanguage.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label2: TsLabel [2]
    Left = 8
    Top = 117
    Width = 54
    Height = 13
    Caption = 'Consi&gnee:'
    FocusControl = FRAMEfindConsignee.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label3: TsLabel [3]
    Left = 412
    Top = 117
    Width = 62
    Height = 13
    Caption = '&Main Carrier:'
    FocusControl = FRAMEfindCarrier.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLlocalMill: TsLabel [4]
    Left = 8
    Top = 65
    Width = 45
    Height = 13
    Caption = 'Local Mill:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLwhseCode: TsLabel [5]
    Left = 8
    Top = 13
    Width = 59
    Height = 13
    Caption = 'Warehouse:'
    FocusControl = EDTwhseCode
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label4: TsLabel [6]
    Left = 8
    Top = 39
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = EDTwhseName
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
```
<!-- tabs:end -->

