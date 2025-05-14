<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRbackAssist` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRbackAssist` code unit defines a form (`TFRAMEbackAssist`) that serves as a user interface for managing back-office assistant data. It provides fields for entering and displaying information such as email, name, login, back-office details, and type of function. The form also includes mechanisms for validating and processing user input, as well as interacting with external services for data retrieval and updates.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the form and its components.
- **SOAP Services**: Used for interacting with external services (`BoAssistServiceUtils` and `BackOfficeServiceUtils`).
- **Database Components**: Includes `TsDBEdit`, `TcxDBImageComboBox`, and `TFRAMEFindEditSOA` for database-bound fields and data manipulation.
- **Custom Components**: Includes `TsLabel`, `TsPanel`, `TsBitBtn`, and `TFRAMEstatusInfo` for UI elements and status display.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `EDTemail` (Email): Text input (database-bound).
  - `EDTname` (Name): Text input (database-bound).
  - `EDTlogin` (Login): Text input (database-bound).
  - `EDTboAssist` (Back Assistant): Text input (database-bound).
  - `ICBOtypeFunction` (Type Function): Dropdown (database-bound).
- **Form Actions and Effects**:
  - `BTNadd`: Adds a new record.
  - `BTNapply`: Applies changes to the current record.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can input and edit back-office assistant details.
- The form validates the login field and retrieves the associated email automatically.
- Users can select a type of function from a dropdown list.
- The form interacts with external services to fetch and update data.

### Main Components:
- **Labels (`TsLabel`)**: Display field names.
- **Database-bound Fields (`TsDBEdit`, `TcxDBImageComboBox`)**: Allow users to input and edit data.
- **Find Edit Frame (`TFRAMEFindEditSOA`)**: Provides a search dialog for selecting back-office details.
- **Status Info Frame (`TFRAMEstatusInfo`)**: Displays status information.
- **Buttons (`TsBitBtn`)**: Trigger actions like adding or applying changes.

### Pseudo-code for Actions and Events:
- `OnChange` event of `EDTlogin`:
  ```pseudo
  if login field value changed then
      set mv_LoginChanged to true
  ```
- `OnExit` event of `EDTlogin`:
  ```pseudo
  if login field value is not empty then
      call m_getEmailFromLogin with login value
  ```
- `m_SetFindOffice` procedure:
  ```pseudo
  configure FRAMEfindOffice with data source and field mappings
  set up FindDialog options for back-office search
  assign data provider service to FindDialog
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The `TFRAMEbackAssist` form is created and initialized with default properties.
   - The `m_SetFindOffice` procedure configures the `FRAMEfindOffice` component for back-office search functionality.
2. **User Interaction**:
   - Users input data into fields like `EDTemail`, `EDTname`, and `EDTlogin`.
   - The `OnChange` and `OnExit` events of `EDTlogin` handle login validation and email retrieval.
3. **Data Processing**:
   - The `m_Validate` function checks the validity of the entered data before saving.
   - External services are used to fetch or update data.

### Required User Data:
- Email
- Name
- Login
- Back-office details
- Type of function

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add Button (`BTNadd`)**:
  - Preconditions: None.
  - Action: Adds a new record.
- **Apply Button (`BTNapply`)**:
  - Preconditions: All required fields must be filled and valid.
  - Action: Saves changes to the current record.

### Available Filters:
- Back-office search filter in `FRAMEfindOffice`.

### Error Messages:
- "Login cannot be empty" if the login field is left blank.
- "Invalid email format" if the email does not match the expected format.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- `EDTlogin`: Must not be empty.
- `EDTemail`: Should be validated for proper email format.

---

## 5. Main Functions:

### Functions:
1. **`m_SetFindOffice`**:
   - Configures the `FRAMEfindOffice` component for back-office search.
2. **`m_getEmailFromLogin`**:
   - Retrieves the email associated with the given login.
3. **`m_Validate`**:
   - Validates the form data before saving.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `BoAssistServiceUtils`
   - **Purpose**: Provides data for back-office assistant management.
2. **Service Name**: `BackOfficeServiceUtils`
   - **Purpose**: Fetches back-office details for the `FRAMEfindOffice` component.

---

## 7. Conditional Fields (Form Logic):

- The `FRAMEfindOffice` component is configured dynamically based on the back-office data source.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP-based service communication.
- **DBClient**: For database operations.

### Custom Components:
- `TFRAMEFindEditSOA`: Provides search functionality.
- `TFRAMEstatusInfo`: Displays status information.

---

## 9. Fields and Validations Listing:

### Fields:
- **Email (`EDTemail`)**: Type: string, required, valid email format.
- **Name (`EDTname`)**: Type: string, required.
- **Login (`EDTlogin`)**: Type: string, required.
- **Back Assistant (`EDTboAssist`)**: Type: string, optional.
- **Type Function (`ICBOtypeFunction`)**: Type: dropdown, optional.

### Mapping:
- `EDTemail` -> Database column: `email`.
- `EDTname` -> Database column: `name`.
- `EDTlogin` -> Database column: `login`.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not provide a complete workflow.)

### Sequence Diagram:
(Not applicable as the code does not provide detailed interactions.)

### Code Snippets:
```delphi
procedure TFRAMEbackAssist.EDTloginChange(Sender: TObject);
begin
  mv_LoginChanged := True;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 697px; height: 196px; background-color: #f0f0f0; font-family: Verdana;">
  <label style="position: absolute; left: 16px; top: 91px;">Email:</label>
  <input type="text" style="position: absolute; left: 100px; top: 91px;" />
  <label style="position: absolute; left: 16px; top: 39px;">Name:</label>
  <input type="text" style="position: absolute; left: 100px; top: 39px;" />
  <label style="position: absolute; left: 16px; top: 13px;">Back Assistant:</label>
  <input type="text" style="position: absolute; left: 100px; top: 13px;" />
  <label style="position: absolute; left: 263px; top: 13px;">Login:</label>
  <input type="text" style="position: absolute; left: 300px; top: 13px;" />
  <label style="position: absolute; left: 446px; top: 13px;">Type Function:</label>
  <select style="position: absolute; left: 530px; top: 13px;"></select>
</div>
```

---

## 11. Important Comments in the Code:

- The `m_SetFindOffice` procedure is critical for configuring the `FRAMEfindOffice` component.
- The `m_getEmailFromLogin` function is essential for retrieving email based on login.

---

## 12. Conclusion:

The `FRbackAssist` code unit provides a robust form for managing back-office assistant data. It integrates with external services for data retrieval and supports validation and dynamic configuration. However, the code lacks detailed error handling and default field values.

---

## 13. Short Summary:

The `FRbackAssist` form manages back-office assistant data, validates user input, and integrates with external services for data retrieval and updates. It includes dynamic search functionality and database-bound fields for seamless data management.#### **FRbackAssist.pas**

```
unit FRbackAssist;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRFindEditSOA, sLabel, Mask, DBCtrls, sDBEdit,
  kneFRStatusInfo, cxGraphics, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxImageComboBox, cxDBEdit;

type
  TFRAMEbackAssist = class(TFRAMEBaseCtrlEditSOA)
    EDTemail: TsDBEdit;
    LBLemail: TsLabel;
    EDTname: TsDBEdit;
    LBLname: TsLabel;
    LBLsalesman: TsLabel;
    EDTboAssist: TsDBEdit;
    LBLbackOffice: TsLabel;
    FRAMEfindOffice: TFRAMEFindEditSOA;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBL1: TsLabel;
    EDTlogin: TsDBEdit;
    ICBOtypeFunction: TcxDBImageComboBox;
    sLabel1: TsLabel;
    procedure EDTloginChange(Sender: TObject);
    procedure EDTloginExit(Sender: TObject);
  private
    { Private declarations }
    mv_LoginChanged: boolean;

    procedure m_SetFindOffice;
    procedure m_getEmailFromLogin(pv_Login: string);

    
  public
    constructor Create(AOwner: TComponent);  override;
    procedure ShowData; override;
    function m_Validate: Boolean;  override;
    { Public declarations }
  end;

var
  FRAMEbackAssist: TFRAMEbackAssist;

implementation

uses
  kneUtils, kneTypes, Global,
  BoAssistServiceUtils, BackOfficeServiceUtils;

{$R *.dfm}

constructor TFRAMEbackAssist.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'BoAssist';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TBoAssistServiceUtils.Create(self);

  //Configura��o do findEdit para o Consignee, Warehouse e Country
  m_SetFindOffice;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

  mv_LoginChanged := False;
end;

procedure TFRAMEbackAssist.m_SetFindOffice;
begin
  with FRAMEfindOffice do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'backOffice';
    EditSettings.FieldNameForDesc := 'boDescrip';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'backoffice';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('boDescrip');

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TBackOfficeServiceUtils.Create(FindDialog);
  end;
```

#### **FRbackAssist.dfm**

```
inherited FRAMEbackAssist: TFRAMEbackAssist
  Width = 697
  Height = 196
  Color = clBtnFace
  Font.Name = 'Verdana'
  ParentColor = False
  object LBLemail: TsLabel [0]
    Left = 16
    Top = 91
    Width = 36
    Height = 13
    Caption = 'Email:'
  end
  object LBLname: TsLabel [1]
    Left = 16
    Top = 39
    Width = 38
    Height = 13
    Caption = 'Name:'
  end
  object LBLsalesman: TsLabel [2]
    Left = 16
    Top = 13
    Width = 88
    Height = 13
    Caption = 'Back Assistant:'
  end
  object LBLbackOffice: TsLabel [3]
    Left = 16
    Top = 65
    Width = 70
    Height = 13
    Caption = 'Back Office:'
  end
  object LBL1: TsLabel [4]
    Left = 263
    Top = 13
    Width = 35
    Height = 13
    Caption = 'Login:'
  end
  object sLabel1: TsLabel [5]
    Left = 446
    Top = 13
    Width = 84
    Height = 13
    Caption = 'Type Function:'
    FocusControl = ICBOtypeFunction
  end
  inherited PNLfooter: TsPanel
    Top = 162
    Width = 697
    TabOrder = 7
    Visible = False
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
```
<!-- tabs:end -->

