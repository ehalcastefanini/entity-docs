<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica para gerenciar os dias de entrega de consignatários (Consignee Delivery Days). Ele permite que os usuários visualizem, editem e configurem os dias e horários permitidos para entrega, utilizando uma grade interativa. O objetivo é facilitar a gestão e edição de dados relacionados a dias de entrega de forma eficiente e organizada.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para desenvolver a aplicação.
  - **Componentes cxGrid e cxEditRepository:** Utilizados para criar a interface gráfica e gerenciar a interação com os dados.
  - **ClientDataSet:** Para manipulação de dados em memória.
  - **SOAPHTTPClient:** Para integração com serviços SOAP.

* **Forma do Componente:**
  - **Grade de Exibição (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `seqNum` (Número Sequencial): Inteiro.
      - `allowed` (Permitido): Checkbox (Sim/Não).
      - `weekDayCode` (Código do Dia da Semana): String.
      - `weekDay` (Dia da Semana): String.
      - `timeBegin` (Hora de Início): Máscara de Hora.
      - `timeEnd` (Hora de Término): Máscara de Hora.
    - **Ações da Grade e seus Efeitos:**
      - Edição de valores diretamente na grade.
      - Validação de campos ao editar.
      - Adição e exclusão de registros.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar novos registros.
  - Editar valores existentes na grade.
  - Validar campos ao editar.
  - Configurar visibilidade e propriedades das colunas.

* **Componentes Principais:**
  - `TcxGridDBTableView`: Exibe os dados em formato de grade.
  - `TClientDataSet`: Gerencia os dados em memória.
  - `TcxEditRepository`: Define editores personalizados para os campos.

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged`:
    ```pseudo
    se valor de célula editado então
        executar validação e atualizar dados
    ```
  - Evento `OnNewRecord`:
    ```pseudo
    se novo registro criado então
        inicializar valores padrão
    ```
  - Validação de Hora:
    ```pseudo
    se valor de hora inválido então
        exibir mensagem de erro
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente com a configuração das colunas e eventos.
  2. Carregamento dos dados no `ClientDataSet`.
  3. Interação do usuário com a grade (edição, adição ou exclusão de registros).
  4. Validação de campos e atualização dos dados.

* **Dados Necessários:**
  - Código do consignatário (`consCode`).
  - Dias da semana e horários permitidos.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar Registro:** Disponível sempre.
  - **Excluir Registro:** Disponível apenas se um registro estiver selecionado.
  - **Editar Registro:** Disponível sempre.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Hora inválida" se o valor do campo de hora não for válido.

* **Valores Padrão dos Campos:**
  - `allowed`: "N" (Não permitido).
  - `timeBegin` e `timeEnd`: Máscara de hora vazia.

* **Validações e Condições dos Campos:**
  - `allowed`: Checkbox com valores "Y" (Sim) e "N" (Não).
  - `timeBegin` e `timeEnd`: Devem seguir o formato de hora `HH:MM`.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `Create`: Configura o componente e inicializa as propriedades.
  - `SetColumnState`: Define o estado de edição de uma coluna.
  - `m_FindWeekDays`: Gerencia a busca de dias da semana.
  - `cxEDTtimePropertiesValidate`: Valida o formato de hora.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - **Nome do Serviço:** WeekDayServiceUtils.
  - **Propósito:** Buscar informações sobre dias da semana.
  - **Tratamento de Erros:** Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxEditRepository`: Para interface gráfica.
  - `SOAPHTTPClient`: Para integração com serviços SOAP.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para o componente.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `seqNum` (Inteiro, obrigatório).
  - `allowed` (Checkbox, obrigatório, valores "Y" ou "N").
  - `weekDayCode` (String, obrigatório).
  - `weekDay` (String, obrigatório).
  - `timeBegin` (Hora, obrigatório, formato `HH:MM`).
  - `timeEnd` (Hora, obrigatório, formato `HH:MM`).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `seqNum` → `seqNum`.
  - `allowed` → `allowed`.
  - `weekDayCode` → `weekDayCode`.
  - `weekDay` → `weekDay`.
  - `timeBegin` → `timeBegin`.
  - `timeEnd` → `timeEnd`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEconsDelDay := TFRAMEconsDelDay.Create(Self);
  FRAMEconsDelDay.Parent := Self;
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <thead>
      <tr>
        <th>SeqNum</th>
        <th>Allowed</th>
        <th>WeekDayCode</th>
        <th>WeekDay</th>
        <th>TimeBegin</th>
        <th>TimeEnd</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>1</td>
        <td>Y</td>
        <td>MON</td>
        <td>Monday</td>
        <td>08:00</td>
        <td>18:00</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração inicial das colunas e eventos no método `Create`.
* Validação de hora no evento `cxEDTtimePropertiesValidate`.

---

## 12. Conclusão:

O código fornece uma solução robusta para gerenciar dias de entrega de consignatários, com uma interface gráfica interativa e validações básicas. No entanto, faltam detalhes sobre tratamento de erros e integração com serviços externos.

---

## 13. Resumo Curto:

Componente Delphi para gerenciar dias de entrega de consignatários, utilizando uma grade interativa com validações e integração com serviços SOAP.#### **FRconsDelDay.pas**

```
unit FRconsDelDay;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEconsDelDay = class(TFRAMEBaseGridEditSOA)
    cxEDTallowed: TcxEditRepositoryCheckBoxItem;
    cxEDTtime: TcxEditRepositoryMaskItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure CDStableBeforeInsert(DataSet: TDataSet);
    procedure CDStableNewRecord(DataSet: TDataSet);
    procedure cxEDTtimePropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    mv_FieldNextVal: Integer;

    procedure m_FindWeekDays(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeWeekDays(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure SetColumnState(pv_ColName: String; pv_Edit: Boolean);
  protected
    { Protected declarations }
    procedure SetKeyEditing(const EditKey: Boolean);  override;
  
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEconsDelDay: TFRAMEconsDelDay;

implementation

uses
  kneInterfaces, kneUtils, kneFindDialogSOA, BaseServiceUtils, Global,
  WeekDayServiceUtils, kneFREditSOA, kneTypes, kneFindDialog, kneDialogFactory,
  kneFGFindUtils;

{$R *.dfm}

{ TFRAMEconsDelDay }

constructor TFRAMEconsDelDay.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode';
  DataPacketName := 'ConsigneeDelDay';
  PropertyName := 'consigneeDelDays';
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    HiddenFields.Add('consCode');
    HiddenFields.Add('updBy');
    HiddenFields.Add('lastUpd');
    // Ordem Campos ............................................................
    OrderFields.Add('seqNum');
    OrderFields.Add('allowed');
    OrderFields.Add('weekDayCode');
    OrderFields.Add('weekDay');
    OrderFields.Add('timeBegin');
    OrderFields.Add('timeEnd');
    // Key Fields ..............................................................
    KeyFields := 'consCode;seqNum';
    // Custom Editors ..........................................................
    AddCustomField('allowed','cxEDTallowed');
    AddCustomField('weekDayCode','cxEDTfind');
    AddCustomField('timeBegin','cxEDTtime');
    AddCustomField('timeEnd','cxEDTtime');
  end; //with

  // Atribui��o dos eventos dos Finds  
  cxEDTfind.Properties.OnButtonClick := m_FindWeekDays;
  //@@@@@
  CDStable.Tag := 5;
```

#### **FRconsDelDay.dfm**

```
inherited FRAMEconsDelDay: TFRAMEconsDelDay
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      DataController.Summary.FooterSummaryItems = <
        item
        end>
      OptionsView.GroupFooters = gfAlwaysVisible
    end
  end
  inherited CDStable: TClientDataSet
    OnNewRecord = CDStableNewRecord
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxEDTallowed: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = 'Y'
      Properties.ValueUnchecked = 'N'
    end
    object cxEDTtime: TcxEditRepositoryMaskItem
      Properties.EditMask = '!90:00;1;_'
      Properties.OnValidate = cxEDTtimePropertiesValidate
    end
  end
end
```
<!-- tabs:end -->

